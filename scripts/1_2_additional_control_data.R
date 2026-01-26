library(sf)
library(dplyr)

# Pfad zu deinem Ordner "regionaldaten"

getwd()
setwd("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/regionaldaten")

# import data on grid-cells

grid_df <- st_read("grid.geojson")

### import data on POI's for different "Regierungsbezirke" in NRW

pois_arnsberg    <- st_read(file.path( "arnsberg",    "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_detmold     <- st_read(file.path( "detmold",     "gis_osm_pois_free_1.shp"), quiet = TRUE)
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



### code auxiliary variable for relevant POI's


table(pois_nrw$fclass)


tags_allday <- c("supermarket")

tags_health <- c("hospital","doctors", "pharmacy")

tags_freetime <- c("park")



#### Daten zu ÖPNV einlesen und dann Bushaltestelle UND/ODER Straßenbahn/Bahn/Bahnhof



pois_nrw_clean <- pois_nrw %>%
  mutate(
    infrastructure_type = case_when(
      fclass %in% tags_allday   ~ "allday",
      fclass %in% tags_health   ~ "health",
      fclass %in% tags_freetime ~ "freetime",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(infrastructure_type))




#### match data with grid-cells

# check CRS

st_crs(pois_nrw_clean)
st_crs(grid_df)


# transform CRS for matching purpose

grid_utm <- st_transform(grid_df, 25832) %>% st_make_valid()


# join datasets

pois_in_grid <- st_join(
  pois_nrw_clean,
  grid_utm[, c("grid_id","munic_id","county_id","lmr_id")],
  join = st_within,
  left = FALSE
)




### first variant -> count of each category by grid-cell

grid_type_long <- pois_in_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id, munic_id, county_id, lmr_id, infrastructure_type) %>%
  summarise(
    n_pois   = n(),
    osm_ids  = list(osm_id),
    fclasses = list(fclass),
    names    = list(name),
    .groups = "drop"
  )
