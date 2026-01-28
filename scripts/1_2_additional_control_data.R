library(sf)
library(dplyr)
library(tidyr)

# Pfad zu deinem Ordner "regionaldaten"

getwd()
setwd("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/regionaldaten")

# import data on grid-cells

grid_df <- st_read("grid.geojson")

### import data on POI's for different "Regierungsbezirke" in NRW

railways_arnsberg    <- st_read(file.path( "arnsberg","gis_osm_pois_free_1.shp.shp"), quiet = TRUE)
pois_detmold     <- st_read(file.path( "detmold",   "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_duesseldorf <- st_read(file.path("duesseldorf", "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_koeln       <- st_read(file.path( "koeln",       "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_muenster    <- st_read(file.path( "muenster",    "gis_osm_pois_free_1.shp"), quiet = TRUE)

# CRS 
pois_arnsberg    <- st_transform(pois_arnsberg,    25832) %>% mutate(bezirk = "arnsberg")
pois_detmold     <- st_transform(pois_detmold,     25832) %>% mutate(bezirk = "detmold")
pois_duesseldorf <- st_transform(pois_duesseldorf, 25832) %>% mutate(bezirk = "duesseldorf")
pois_koeln       <- st_transform(pois_koeln,       25832) %>% mutate(bezirk = "koeln")
pois_muenster    <- st_transform(pois_muenster,    25832) %>% mutate(bezirk = "muenster")


# bind datasets 

pois_nrw <- bind_rows(
  pois_arnsberg, pois_detmold, pois_duesseldorf, pois_koeln, pois_muenster
)


# filter for relevant POI's


pois_nrw_relevant <- pois_nrw %>%
  filter(
    fclass %in% c(
      "supermarket",
      "hospital",
      "doctors",
      "pharmacy",
      "park"
      )
  )



#### match data with grid-cells

# check CRS

st_crs(pois_nrw_relevant)
st_crs(grid_df)


# adjust CRS for matching purpose for alignment between data-sets

grid_utm <- st_transform(grid_df, 25832) %>% st_make_valid()
pois_utm <- st_transform(pois_nrw_relevant, 25832)


# join pois-dataset with grid-ids

pois_in_grid <- st_join(
  pois_utm,
  grid_utm[, c("grid_id")],   
  join = st_within,
  left = FALSE
)


## create dummy-variables for controls for each POI
# 1 if POI is present in grid-cell, 0 otherwise

wanted <- c("doctors", "hospital", "park", "pharmacy", "supermarket") # define relvant POI's

grid_dummies <- pois_in_grid %>% # code dummies in pois-dataset 
  st_drop_geometry() %>%
  filter(fclass %in% wanted) %>%
  distinct(grid_id, fclass) %>%        
  mutate(value = 1L) %>%
  pivot_wider(names_from = fclass, values_from = value, values_fill = 0L)


grid_with_dummies_utm <- grid_utm %>% # join them with grid-ids
  left_join(grid_dummies, by = "grid_id") %>%
  mutate(across(any_of(wanted), ~coalesce(.x, 0L)))
