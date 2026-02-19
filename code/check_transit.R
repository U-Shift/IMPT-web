library(sf)
library(dplyr)
library(here)

# Check 0800.geojson
msg <- tryCatch(
    {
        d8 <- st_read(here("www/data/sample/0800.geojson"), quiet = TRUE)
        print(head(d8))
        print(colnames(d8))
    },
    error = function(e) print(e)
)

# Check POIs groups
# msg <- tryCatch(
#  {
#     pois <- st_read(here("data/pois_osm2024.gpkg"), quiet = TRUE) # the stops are not here. I will add later and replace the path.
#    print(unique(pois$group))
# },
# error = function(e) print(e)
# )
