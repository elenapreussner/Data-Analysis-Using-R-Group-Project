#########################
##### preliminaries #####
#########################

### load necessary packages

library(tidyverse)
library(readxl)
library(sf)


setwd("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis")

#### import various data-sets ####

# raw school dataset

schools <- read_xlsx("data/school_data/school_data.xlsx")

# housing data

housing_data <- read.csv("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/CampusFile_HK_2022.csv")
housing_data <- read_csv("~/Uni/Data Analysis Using R/CampusFile_HK_2022.csv")

# data on SSI
ssi_data <- read_csv2("data/school_data/2022_social_index.csv")


# data for neighborhood controls

neighborhood_data <- read_xlsx("data/neighborhood_data/neighborhood_controls.xlsx", na = c("â€“"))


# data for district/regional controls per "Regierungsbezirk

pois_arnsberg    <- st_read(file.path("data/regional_district_data/arnsberg", "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_detmold     <- st_read(file.path("data/regional_district_data/detmold", "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_duesseldorf <- st_read(file.path("data/regional_district_data/duesseldorf", "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_koeln       <- st_read(file.path("data/regional_district_data/koeln", "gis_osm_pois_free_1.shp"), quiet = TRUE)
pois_muenster    <- st_read(file.path("data/regional_district_data/muenster", "gis_osm_pois_free_1.shp"), quiet = TRUE)

# grid-cell ID data-set

grid_df <- st_read("grid.geojson")



#######################
##### preperation #####
#######################


####################
## school dataset ##
####################

# rename school ID

ssi_data <- ssi_data %>%
  rename(school_ID = Schulnummer,
         ssi = Sozialindexstufe)


# get information on SSI

schools <- schools %>%
  left_join(
    ssi_data %>% 
      select(school_ID, ssi),
    by = "school_ID"
  ) 


#### filter school data regarding relevant school types (secondary schools)

schools <- schools %>%
  filter(school_type %in% c(4, 10, 14, 15, 20))


#### separate grid-id for the treatment assignent

schools <- schools %>%
  separate(ergg_1km, c("x", "y"), "_", remove = FALSE, convert = TRUE)


##################
## housing data ##
##################

##### housing data


### filter for houses located in NRW


housing_data_NRW <- housing_data %>%
  filter(blid == "North Rhine-Westphalia")

## remove NA's on the grid-cell ID

housing_data_NRW <- housing_data_NRW %>%
  filter(ergg_1km != -9)


#### recode controls




#######################
## neighborhood data ##
#######################
# NA's are already correctly interpreted, nothing more has to be done here for preperation




###################
## district data ##
###################


##### set CRS 
pois_arnsberg    <- st_transform(pois_arnsberg,    25832) %>% mutate(bezirk = "arnsberg")
pois_detmold     <- st_transform(pois_detmold,     25832) %>% mutate(bezirk = "detmold")
pois_duesseldorf <- st_transform(pois_duesseldorf, 25832) %>% mutate(bezirk = "duesseldorf")
pois_koeln       <- st_transform(pois_koeln,       25832) %>% mutate(bezirk = "koeln")
pois_muenster    <- st_transform(pois_muenster,    25832) %>% mutate(bezirk = "muenster")


##### bind regional-datasets into one NRW-dataset

pois_nrw <- bind_rows(
  pois_arnsberg, pois_detmold, pois_duesseldorf, pois_koeln, pois_muenster
)

##### filter for relevant POI's
# to keep it simlpe, we have chosen a few central district characteristics to control for their presence/absence in each grid-cell

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


##### join pois-dataset with grid-ids

pois_in_grid <- st_join(
  pois_utm,
  grid_utm[, c("grid_id")],   
  join = st_within,
  left = FALSE
)


## create dummy-variables for controls for each POI
# 1 if POI is present in grid-cell, 0 otherwise

grid__district_dummies <- pois_in_grid %>%
  st_drop_geometry() %>%
  distinct(grid_id, fclass) %>%       
  mutate(value = 1L) %>%
  pivot_wider(names_from = fclass, values_from = value, values_fill = 0L)


# join those district dummies back with the full grid-cell dataset and code missings to zero

grid_full_with_district_dummies <- grid_utm %>%
  left_join(grid__district_dummies, by = "grid_id") %>%
  mutate(
    across(
      setdiff(names(grid__district_dummies), "grid_id"),
      ~coalesce(.x, 0L)
    )
  )


#########################################################
#### joining all information except treatment status ####
#########################################################














