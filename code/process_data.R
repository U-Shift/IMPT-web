library(sf)
library(dplyr)
library(jsonlite)
library(tidyr)
library(readxl)
library(here)

# 1. Load Geometries
message("Loading Geometries...")
freg_geom <- st_read(here("data/freguesias_2024.gpkg"), quiet = TRUE) %>%
  st_transform(4326) %>%
  st_make_valid() %>%
  mutate(dtmnfr = as.character(dtmnfr))

grid <- st_read(here("data/grelha_h3_r8.gpkg"), quiet = TRUE) %>%
  st_transform(4326) %>%
  mutate(hex_id = as.character(id)) %>%
  select(hex_id, geom)

# 2. Infra Ratio
if (file.exists(here("data/freguesias_infrastructure_ratio.rds"))) {
  infra_data <- readRDS(here("data/freguesias_infrastructure_ratio.rds")) %>%
    mutate(dtmnfr = as.character(dtmnfr)) %>%
    select(dtmnfr, pedpath_to_road_ratio, cycleway_to_road_ratio)
} else {
  infra_data <- data.frame(dtmnfr = character(), pedpath_to_road_ratio = numeric(), cycleway_to_road_ratio = numeric())
}

# 3. Census Data
census_pts <- st_read(here("data/census24_points.gpkg"), quiet = TRUE) %>% st_transform(4326)
inter_pts <- st_within(census_pts, grid)
census_pts$hex_id <- grid$hex_id[sapply(inter_pts, function(x) if (length(x) > 0) x[1] else NA)]

hex_census <- census_pts %>%
  st_drop_geometry() %>%
  filter(!is.na(hex_id)) %>%
  group_by(hex_id) %>%
  summarise(
    pop_t = sum(N_INDIVIDUOS, na.rm = TRUE),
    pop_w = sum(N_INDIVIDUOS_M, na.rm = TRUE),
    pop_y = sum(N_INDIVIDUOS_0_14, na.rm = TRUE),
    pop_e = sum(N_INDIVIDUOS_65_OU_MAIS, na.rm = TRUE),
    houses_p45 = sum(N_EDIFICIOS_CONSTR_ANTES_1945, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(pop_total = pop_t)

# 4. POIs
pois <- st_read(here("data/pois_osm2024.gpkg"), quiet = TRUE) %>% st_transform(4326)
inter_pois <- st_within(pois, grid)
pois$hex_id <- grid$hex_id[sapply(inter_pois, function(x) if (length(x) > 0) x[1] else NA)]

hex_pois <- pois %>%
  st_drop_geometry() %>%
  filter(!is.na(hex_id)) %>%
  group_by(hex_id, group) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = count, values_fill = 0) %>%
  rename_with(~ paste0("poi_", .), -hex_id)

# 5. TTM Scenarios
process_ttm <- function(path, dests, tag) {
  if (!file.exists(path)) {
    return(data.frame(hex_id = character(), min_t = numeric()))
  }
  readRDS(path) %>%
    filter(to_id %in% dests) %>%
    group_by(from_id) %>%
    summarise(min_t = min(travel_time_p50, na.rm = TRUE)) %>%
    rename(hex_id = from_id, !!tag := min_t) %>%
    mutate(hex_id = as.character(hex_id), !!tag := pmin(get(tag), 60))
}

poi_h_dest <- unique(pois$hex_id[pois$group == "healthcare" & !is.na(pois$hex_id)])
res_car <- process_ttm(here("data/ttm_h3_res8/ttm_car_60min_202602040800.rds"), poi_h_dest, "time_car")
res_peak <- process_ttm(here("data/ttm_h3_res8/ttm_transit_60min_202602040800_1transfers.rds"), poi_h_dest, "time_pt_peak")
res_off <- process_ttm(here("data/ttm_h3_res8/ttm_transit_60min_202602082000_1transfers.rds"), poi_h_dest, "time_pt_off")
res_night <- process_ttm(here("data/ttm_h3_res8/ttm_transit_60min_202602040300_1transfers.rds"), poi_h_dest, "time_pt_night")

hex_stats <- grid %>%
  left_join(res_car, by = "hex_id") %>%
  left_join(res_peak, by = "hex_id") %>%
  left_join(res_off, by = "hex_id") %>%
  left_join(res_night, by = "hex_id") %>%
  left_join(hex_census, by = "hex_id") %>%
  left_join(hex_pois, by = "hex_id") %>%
  mutate(across(starts_with("time_"), ~ replace_na(., 60))) %>%
  mutate(accessibility_gap = time_pt_peak - time_car) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# 6. Aggregations
grid_centroids <- st_centroid(grid)
cent_inter <- st_within(grid_centroids, freg_geom)
grid_to_freg <- data.frame(hex_id = grid$hex_id, dtmnfr = freg_geom$dtmnfr[sapply(cent_inter, function(x) if (length(x) > 0) x[1] else NA)])

poi_cols <- grep("^poi_", names(hex_stats), value = TRUE)
time_cols <- grep("^time_", names(hex_stats), value = TRUE)

freg_agg <- hex_stats %>%
  st_drop_geometry() %>%
  inner_join(grid_to_freg, by = "hex_id") %>%
  group_by(dtmnfr) %>%
  summarise(
    ag = weighted.mean(accessibility_gap, pop_total + 1, na.rm = TRUE),
    pt = sum(pop_total, na.rm = TRUE),
    pw = sum(pop_w, na.rm = TRUE),
    py = sum(pop_y, na.rm = TRUE),
    pe = sum(pop_e, na.rm = TRUE),
    hp = sum(houses_p45, na.rm = TRUE),
    across(all_of(time_cols), \(x) weighted.mean(x, pop_total + 1, na.rm = TRUE)),
    across(all_of(poi_cols), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  )

v_freg <- readRDS(here("data/imob_vehicles_freg.Rds")) %>%
  mutate(dtmnfr = as.character(dicofre)) %>%
  select(dtmnfr, total_motor_vehicles_per_hh, pct_hh_no_vehicle, avg_bicycles)

od_freg <- st_read(here("data/od_freguesias_jittered_2024.gpkg"), quiet = TRUE) %>%
  st_drop_geometry() %>%
  mutate(dtmnfr = as.character(Origin_dicofre24)) %>%
  group_by(dtmnfr) %>%
  summarise(
    tot = sum(Total, na.rm = TRUE),
    sc = sum(Car, na.rm = TRUE) / tot,
    sp = sum(PTransit, na.rm = TRUE) / tot,
    ss = (sum(Walk, na.rm = TRUE) + sum(Bike, na.rm = TRUE)) / tot,
    .groups = "drop"
  )

freg_final <- freg_geom %>%
  inner_join(freg_agg, by = "dtmnfr") %>%
  left_join(v_freg, by = "dtmnfr") %>%
  left_join(od_freg, by = "dtmnfr") %>%
  left_join(infra_data, by = "dtmnfr") %>%
  mutate(
    pop_total = pt,
    pop_density = ifelse(area_ha > 0, pop_total / (area_ha / 100), 0),
    pct_women = pw / pop_total, pct_youth = py / pop_total, pct_elderly = pe / pop_total, pct_pre_1945 = hp / pop_total,
    mobility_poverty_index = ((time_pt_peak / 60) * 0.4 + sc * 0.3 + (pct_hh_no_vehicle / 100) * 0.3) * 100
  ) %>%
  rename(share_car = sc, share_pt = sp, share_soft = ss, accessibility_gap = ag) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

# 7. Income
income_df <- read_excel(here("data/ERendimentoNLocal2022.xlsx"), sheet = "Sujeitos Passivos_pub_2022", skip = 3)
income_clean <- income_df %>%
  select(code = 1, type = 2, income = 7, gini = 36) %>%
  filter(type == "Município") %>%
  mutate(code = as.character(code), income = as.numeric(income), gini = as.numeric(gini))

# 8. Mun Agg
mun_agg <- freg_final %>%
  st_drop_geometry() %>%
  group_by(municipio) %>%
  summarise(
    ag = weighted.mean(accessibility_gap, pop_total + 1, na.rm = TRUE),
    sc = weighted.mean(share_car, pop_total + 1, na.rm = TRUE),
    sp = weighted.mean(share_pt, pop_total + 1, na.rm = TRUE),
    ss = weighted.mean(share_soft, pop_total + 1, na.rm = TRUE),
    vm = weighted.mean(total_motor_vehicles_per_hh, pop_total + 1, na.rm = TRUE),
    vb = weighted.mean(avg_bicycles, pop_total + 1, na.rm = TRUE),
    vn = weighted.mean(pct_hh_no_vehicle, pop_total + 1, na.rm = TRUE),
    vi = weighted.mean(mobility_poverty_index, pop_total + 1, na.rm = TRUE),
    pt = sum(pop_total, na.rm = TRUE),
    ah = sum(area_ha, na.rm = TRUE),
    n2 = first(nuts2),
    across(all_of(time_cols), \(x) weighted.mean(x, pop_total + 1, na.rm = TRUE)),
    across(all_of(poi_cols), \(x) sum(x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rename(
    share_car = sc, share_pt = sp, share_soft = ss, total_motor_vehicles_per_hh = vm,
    avg_bicycles = vb, pct_hh_no_vehicle = vn, mobility_poverty_index = vi, pop_total = pt, area_ha = ah, nuts2 = n2, accessibility_gap = ag
  ) %>%
  mutate(pop_density = ifelse(area_ha > 0, pop_total / (area_ha / 100), 0))

mun_codes <- freg_final %>%
  st_drop_geometry() %>%
  group_by(municipio) %>%
  summarise(code = substr(first(dtmnfr), 1, 4))
mun_final_data <- mun_agg %>%
  left_join(mun_codes, by = "municipio") %>%
  left_join(income_clean, by = "code")
mun_geom <- freg_geom %>%
  group_by(municipio) %>%
  summarise(geom = st_union(geom), .groups = "drop") %>%
  st_make_valid()
mun_final <- mun_geom %>% inner_join(mun_final_data, by = "municipio")

# 9. Rounded and Export
freg_final <- freg_final %>% mutate(across(where(is.numeric), ~ round(., 2)))
mun_final <- mun_final %>% mutate(across(where(is.numeric), ~ round(., 2)))
hex_stats <- hex_stats %>% mutate(across(where(is.numeric), ~ round(., 2)))

out_dir <- here("dashboard/public/data")
st_write(freg_final, file.path(out_dir, "freguesias_data.json"), delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
st_write(mun_final, file.path(out_dir, "municipios_data.json"), delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
st_write(hex_stats %>% st_simplify(dTolerance = 0.0005), file.path(out_dir, "hex_data.json"), delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")
st_write(mun_geom, file.path(out_dir, "municipios_limits.json"), delete_dsn = TRUE, quiet = TRUE, driver = "GeoJSON")

message("Done!")
