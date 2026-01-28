setwd("C:/Users/julia/OneDrive/MASTER/3. Semester/R/Data")
library(readxl)
housing_data <- read_excel("CampusFile_HK_2022.xlsx")

housing_data_NRW <- housing_data %>%
  filter(blid == "North Rhine-Westphalia")

#variante 1
housing_data_NRW <- housing_data_NRW %>%
  filter(
    ergg_1km != -9,
    !is.na(baujahr),
    badezimmer != "Other missing", 
    grundstuecksflaeche != "Implausible value"
  )

#Variante 2
housing_data_NRW <- housing_data_NRW %>%
  mutate(
    ergg_1km = ifelse(ergg_1km == -9, NA, ergg_1km),
    badezimmer = ifelse(badezimmer == "Other missing", NA, badezimmer),
    grundstuecksflaeche = ifelse(grundstuecksflaeche == "Implausible value", NA, grundstuecksflaeche)
  )

housing_data_NRW <- housing_data_NRW %>%
  drop_na()

###
housing_data_NRW_control <- housing_data_NRW %>%
  select(
    wohnflaeche,
    grundstuecksflaeche,
    zimmeranzahl,
    badezimmer,
    baujahr,
    keller,
    price_sqm,
    ergg_1km
  )


