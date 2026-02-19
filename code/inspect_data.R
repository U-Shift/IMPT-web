library(sf)
library(dplyr)
library(here)

# Load Hex Grid
grid <- st_read(here("data/grelha_h3_r8.gpkg"), quiet = TRUE)
print("Grid columns:")
print(colnames(grid))

# Load TTM (Transit example)
ttm_file <- here("data/ttm_h3_res8/ttm_transit_60min_202602040800_1transfers.rds")
if (file.exists(ttm_file)) {
  ttm <- readRDS(ttm_file)
  print("TTM columns:")
  print(colnames(ttm))
  print(head(ttm))
} else {
  print("TTM file not found")
  # Try loading Car TTM if Transit one fails
  ttm_file <- here("data/ttm_h3_res8/ttm_car_60min_202602040800.rds")
  if (file.exists(ttm_file)) {
    ttm <- readRDS(ttm_file)
    print("Car TTM columns:")
    print(colnames(ttm))
    print(head(ttm))
  }
}

# Load POIs
pois <- st_read(here("data/pois_osm2024.gpkg"), quiet = TRUE)
print("POIs columns:")
print(colnames(pois))
print(table(pois$group))
