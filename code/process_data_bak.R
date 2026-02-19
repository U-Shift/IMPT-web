library(sf)
library(dplyr)
library(jsonlite)

# 1. Load Geometries
message("Loading Geometries...")
freguesias <- st_read("data/freguesias_2024.gpkg", quiet = TRUE) |>
  st_transform(4326) |>
  st_make_valid() |>
  st_simplify(dTolerance = 0.001, preserveTopology = TRUE) |> # simplify more aggressively
  select(dtmnfr, municipio, freguesia) |>
  # Deduplicate geometries
  group_by(dtmnfr, municipio, freguesia) |>
  summarise(geom = st_union(geom), .groups = "drop") |>
  st_make_valid()

grid <- st_read("data/grelha_h3_r8.gpkg", quiet = TRUE) |>
  st_transform(4326)

# 2. Spatial Join: Assign Hex to District
# Use centroids for faster/cleaner assignment
grid_centroids <- st_centroid(grid)
grid_district <- st_join(grid_centroids, freguesias, join = st_within) |>
  st_drop_geometry() |>
  select(id, dtmnfr) |>
  filter(!is.na(dtmnfr))

# 3. Load POIs & Identify Destination Hexes
message("Processing POIs...")
pois <- st_read("data/pois_osm2024.gpkg", quiet = TRUE)
health_pois <- pois |> filter(group == "healthcare")

# Find which hex contains health POIs
# We use st_join with st_intersects
poi_hex <- st_join(health_pois, grid, join = st_intersects) |>
  st_drop_geometry() |>
  select(id) |> # id is the hex id
  distinct() |>
  filter(!is.na(id)) |>
  rename(dest_id = id)

dest_ids <- poi_hex$dest_id

# 4. Process TTMs
process_ttm <- function(file_path, dest_ids, mode_name, max_time = 60) {
  if (!file.exists(file_path)) {
    return(NULL)
  }

  message(paste("Processing TTM:", mode_name))
  ttm <- readRDS(file_path)

  # Filter only trips to relevant destinations
  ttm_filtered <- ttm |>
    filter(to_id %in% dest_ids)

  # Find min time per origin
  ttm_min <- ttm_filtered |>
    group_by(from_id) |>
    summarise(min_time = min(travel_time_p50, na.rm = TRUE)) |>
    ungroup()

  # Ensure types match for join
  grid_district$id <- as.character(grid_district$id)
  ttm_min$from_id <- as.character(ttm_min$from_id)

  # Join with all grid cells in districts
  res <- grid_district |>
    left_join(ttm_min, by = c("id" = "from_id")) |>
    mutate(min_time = ifelse(is.na(min_time), max_time, min_time)) |>
    # Cap at max_time
    mutate(min_time = ifelse(min_time > max_time, max_time, min_time)) |>
    group_by(dtmnfr) |>
    summarise(
      avg_time = mean(min_time, na.rm = TRUE)
    ) |>
    rename(!!paste0("time_", mode_name) := avg_time)

  return(res)
}

# TTM Paths
ttm_car <- "data/ttm_h3_res8/ttm_car_60min_202602040800.rds"
ttm_pt <- "data/ttm_h3_res8/ttm_transit_60min_202602040800_1transfers.rds"
ttm_walk <- "data/ttm_h3_res8/ttm_walk_60min_202602040800.rds"

res_car <- process_ttm(ttm_car, dest_ids, "car")
res_pt <- process_ttm(ttm_pt, dest_ids, "pt")
res_walk <- process_ttm(ttm_walk, dest_ids, "walk")

# 5. Process OD Data (for Modal Share)
message("Processing OD Data...")
od <- st_read("data/od_freguesias_jittered_2024.gpkg", quiet = TRUE)

# OD is likely linestrings, but valid OD table
# Columns: Total, Walk, Bike, Car, PTransit, Origin_dicofre24
od_stats <- od |>
  st_drop_geometry() |>
  group_by(Origin_dicofre24) |>
  summarise(
    trips_total = sum(Total, na.rm = TRUE),
    trips_car = sum(Car, na.rm = TRUE),
    trips_pt = sum(PTransit, na.rm = TRUE),
    trips_walk = sum(Walk, na.rm = TRUE),
    trips_bike = sum(Bike, na.rm = TRUE)
  ) |>
  mutate(
    share_car = trips_car / trips_total,
    share_pt = trips_pt / trips_total,
    share_walk = trips_walk / trips_total,
    share_bike = trips_bike / trips_total
  ) |>
  rename(dtmnfr = Origin_dicofre24)

# 6. Combine All Data
message("Merging Data...")
# Ensure dtmnfr matches types too
freguesias$dtmnfr <- as.character(freguesias$dtmnfr)

if (!is.null(res_car)) {
  res_car$dtmnfr <- as.character(res_car$dtmnfr)
  final_stats <- freguesias |> left_join(res_car, by = "dtmnfr")
} else {
  final_stats <- freguesias
}

if (!is.null(res_pt)) {
  res_pt$dtmnfr <- as.character(res_pt$dtmnfr)
  final_stats <- final_stats |> left_join(res_pt, by = "dtmnfr")
}

if (!is.null(res_walk)) {
  res_walk$dtmnfr <- as.character(res_walk$dtmnfr)
  final_stats <- final_stats |> left_join(res_walk, by = "dtmnfr")
}

od_stats$dtmnfr <- as.character(od_stats$dtmnfr)
final_stats <- final_stats |> left_join(od_stats, by = "dtmnfr")

# Handle missing values (if any district has no data)
final_stats <- final_stats |>
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0.1, .)))

# Calculate Mobility Poverty Indicator (Example)
if ("time_pt" %in% names(final_stats) && "time_car" %in% names(final_stats)) {
  final_stats <- final_stats |>
    mutate(
      accessibility_gap = time_pt - time_car,
      mobility_poverty_index = (time_pt / 60) * 0.5 + (share_car) * 0.5 # Dummy index
    )
}

# 7. Export to GeoJSON
message("Exporting to GeoJSON...")
if (!dir.exists("dashboard/src/assets")) dir.create("dashboard/src/assets", recursive = TRUE)
st_write(final_stats, "dashboard/src/assets/districts_data.json", delete_dsn = TRUE)

message("Done!")
