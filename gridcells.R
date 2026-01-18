#########################
##### preliminaries #####
#########################


### load necessary packages

load_packages <- function() {
  pkgs <- c(
    # load data
    "haven",
    "readxl",
    "readr",
    
    # Data manipulation
    "tidyverse",
    "lubridate",
    "tidyr",

    # Matching & causal inference
    "MatchIt",
    "cobalt",
    "WeightIt",
    "did",
    
    # Regression analysis
    "fixest",
    "estimatr",
    "stats",
    "lmtest",
    "sandwich",
    "marginaleffects",
    
    # Tables & reporting
    "modelsummary",
    "kableExtra",
    "broom",
    
    # Visualization
    "ggplot2",
    "patchwork"
  )
  
  invisible(lapply(pkgs, library, character.only = TRUE))
}


load_packages()


#### import data

# Version for Elena
schulen <- read_xlsx("~/Uni/Data Analysis Using R/school_data.xlsx")
hauspreise <- read_csv("~/Uni/Data Analysis Using R/CampusFile_HK_2022.csv")

# version for Benedikt
setwd("C:/Users/bened/OneDrive/Desktop/Uni/Master Economic Policy Consulting/Wintersemester 2025-26/Data Analysis/Data-Analysis-Using-R-Group-Project")

schulen <- read_xlsx("school_data.xlsx")

distances <- read_xlsx("distance_to_schools.xlsx")

ssi_data <- read_csv2("2022_social_index.csv")

pupils_data <- read_xlsx("number_pupils.xlsx")

housing_data <- read_csv("CampusFile_HK_2022.csv")





############################
##### Prepare datasets #####
############################


#### create full school dataset

ssi_data <- ssi_data %>%
  rename(school_ID = Schulnummer)

schulen <- schulen %>%
  left_join(
    ssi_data %>% 
      select(school_ID, Sozialindexstufe),
    by = "school_ID"
  )

schulen <- schulen %>%
  left_join(
    pupils_data %>% 
      select(school_ID, number_pupils),
    by = "school_ID"
  )


#### clean-up housing data - use only the houses with information on grid-cells

## recode NA's on grid


housing_data <- housing_data %>%
  mutate(
    ergg_1km = if_else(ergg_1km == "-9", NA_character_, ergg_1km)
  )


## exclude houses with NA's


housing_data_clean <- housing_data %>%
  filter(!is.na(ergg_1km))

################################################
##### Identification of treated grid-cells #####
################################################

#### define relevant school types along the 'Abitur' availability


abitur <- c(20, 15)                                    
kein_abitur <- c(4, 10, 14)


schulen_abi <- schulen %>%
  filter(school_type %in% c(15, 20))


schulen_alle <- schulen %>%
  filter(school_type %in% c(4, 10, 14, 15, 20))

view(treated_cells)


#### create treated-cell dataset for 'Abitur'-schools

treated_cells <- schulen_abi %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1)) %>%
      transmute(
        x = x + dx,
        y = y + dy,
        school_tag = if_else(dx == 0 & dy == 0,
                             as.character(school_ID),
                             paste0(school_ID, "t"))
      )
  ) %>%
  ungroup()

treated_cells <- treated_cells %>%
  mutate(ergg_1km = paste0(x, "_", y))

treated_cells_unique <- treated_cells %>%
  distinct(ergg_1km, .keep_all = TRUE)



#### create buffer-cells for the exclusion

buffer_cells <- schulen_abi %>%
  rowwise() %>%
  reframe(
    tibble(
      x = c(
        x+2, x+2, x+2, x+2, x+2,
        x,   x+1, x-1, x-2,
        x-2, x-2, x-2, x-2,
        x,   x-1, x+1
      ),
      y = c(
        y,   y+1, y+2, y-1, y-2,
        y-2, y-2, y-2, y-2,
        y,   y-1, y+1, y+2,
        y+2, y+2, y+2
      )
    )
  ) %>%
  ungroup() %>%
  distinct()

buffer_cells <- buffer_cells %>%
  mutate(ergg_1km = paste0(x, "_", y))


#### check for overlaps

any(treated_cells_unique$ergg_1km %in% buffer_cells$ergg_1km)


overlap_ids <- intersect(
  treated_cells_unique$ergg_1km,
  buffer_cells$ergg_1km
)

length(overlap_ids)


#### PROBLEM: 2859 overlapping treated cells with buffer


#### match datasets using grid-identifyer

all_cells <- bind_rows(
  treated_cells_unique %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L,
      buffer  = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag)
    ),
  buffer_cells %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,
      buffer  = 1L,
      source  = "buffer_cells",
      school_tag = NA_character_
    )
) %>%
  group_by(ergg_1km, x, y) %>%
  summarise(
    treated = max(treated),
    buffer  = max(buffer),
    source  = paste(sort(unique(source)), collapse = " + "),
    school_tag = if (all(is.na(school_tag))) NA_character_
    else paste(sort(unique(na.omit(school_tag))), collapse = ", "),
    .groups = "drop"
  )


all_cells %>%
  count(ergg_1km) %>%
  filter(n > 1)



##### data aggregation seems successful ########

#### import school data


all_cells_schools <- all_cells %>%
  mutate(
    # nur echte Schulzellen bekommen eine Join-ID
    school_ID_lookup = if_else(
      grepl("t$", school_tag),
      NA_character_,
      school_tag
    )
  ) %>%
  left_join(
    schulen_abi %>%
      mutate(school_ID = as.character(school_ID)),
    by = c("school_ID_lookup" = "school_ID")
  )


#### match information with housing data

full_dataset <- housing_data_clean %>%
  left_join(all_cells_schools, by = "ergg_1km")



##################################################
##### preparation final dataset for anaylsis #####
##################################################




# Funktion: Finde 5x5 Nachbarn mit Distanz

finde_nachbarn <- function(df, max_dist = 2) {
  df %>%
    separate(ergg_1km, c("x", "y"), "_", remove = FALSE, convert = TRUE) %>%
    rowwise() %>%
    reframe(
      expand.grid(dx = -max_dist:max_dist, dy = -max_dist:max_dist) %>%
        mutate(
          ergg_1km = paste0(x + dx, "_", y + dy),
          distanz = pmax(abs(dx), abs(dy))
        )
    )
}

# Alle Zellen (Schule + Nachbarn) für beide Schultypen finden

zellen_abitur <- schulen %>%
  filter(school_type %in% abitur) %>%
  finde_nachbarn()


zellen_kein_abitur <- schulen %>%
  filter(school_type %in% kein_abitur) %>%
  finde_nachbarn()

# Treatment-Zellen auf Überlappungen prüfen

alle_treatment <- bind_rows(
  zellen_abitur %>% filter(distanz <= 1),
  zellen_kein_abitur %>% filter(distanz <= 1)
)


view(alle_treatment)

view(saubere_treatment)
saubere_treatment <- alle_treatment %>%
  count(ergg_1km) %>%
  filter(n == 1) %>%
  pull(ergg_1km)

# Treatment nach Typ aufteilen

treatment_abitur <- zellen_abitur %>%
  filter(distanz <= 1, ergg_1km %in% saubere_treatment) %>%
  select(ergg_1km) %>%
  mutate(treat_abitur = 1)

treatment_kein_abitur <- zellen_kein_abitur %>%
  filter(distanz <= 1, ergg_1km %in% saubere_treatment) %>%
  select(ergg_1km) %>%
  mutate(treat_kein_abitur = 1)

# Buffer (distanz == 2)

buffer <- bind_rows(
  zellen_abitur %>% filter(distanz == 2),
  zellen_kein_abitur %>% filter(distanz == 2)
) %>%
  distinct(ergg_1km) %>%
  mutate(buffer = 1)

# Alles zusammenführen

grid_treatment <- full_join(treatment_abitur, treatment_kein_abitur, by = "ergg_1km") %>%
  full_join(buffer, by = "ergg_1km") %>%
  replace_na(list(treat_abitur = 0, treat_kein_abitur = 0, buffer = 0))

view(grid_treatment)




# Mit Hauspreisen verknüpfen
haeuser <- hauspreise %>%
  filter(ergg_1km != -9) %>%
  left_join(grid_treatment, by = "ergg_1km") %>%
  replace_na(list(treat_abitur = 0, treat_kein_abitur = 0, buffer = 0)) %>%
  select(plz, kaufpreis, ergg_1km, treat_abitur, treat_kein_abitur, buffer)

# Finaler Analyse-Datensatz (ohne Buffer)
haeuser_final <- haeuser %>% 
  filter(buffer == 0) %>% 
  select(plz, kaufpreis, ergg_1km, treat_abitur, treat_kein_abitur) #anpassen!

# =============================================================================
# DIAGNOSTIK
# =============================================================================

cat("\n=== GRID-ZELLEN ===\n")
cat("Treatment (Abitur):", sum(grid_treatment$treat_abitur), "\n")
cat("Treatment (Kein Abitur):", sum(grid_treatment$treat_kein_abitur), "\n")
cat("Buffer-Zonen:", sum(grid_treatment$buffer), "\n")

anzahl_unique_treatment <- n_distinct(alle_treatment$ergg_1km)
cat("Ausgeschlossen (Überlappungen):", anzahl_unique_treatment - length(saubere_treatment), 
    sprintf("(%.1f%%)\n", 100 * (anzahl_unique_treatment - length(saubere_treatment)) / anzahl_unique_treatment))

cat("\n=== HÄUSER (vor Buffer-Filter) ===\n")
cat("Gesamt:", nrow(haeuser), "\n")
cat("Bei Abitur-Schulen:", sum(haeuser$treat_abitur), "\n")
cat("Bei Nicht-Abitur-Schulen:", sum(haeuser$treat_kein_abitur), "\n")
cat("In Buffer-Zone:", sum(haeuser$buffer), "\n")
cat("Control:", sum(haeuser$treat_abitur == 0 & haeuser$treat_kein_abitur == 0 & haeuser$buffer == 0), "\n")

cat("\n=== TREATMENT-VERLUST DURCH BUFFER ===\n")
urspruenglich_abitur <- sum(grid_treatment$treat_abitur == 1)
urspruenglich_nicht_abitur <- sum(grid_treatment$treat_kein_abitur == 1)
abitur_und_buffer <- sum(grid_treatment$treat_abitur == 1 & grid_treatment$buffer == 1)
nicht_abitur_und_buffer <- sum(grid_treatment$treat_kein_abitur == 1 & grid_treatment$buffer == 1)

cat("Abitur ursprünglich:", urspruenglich_abitur, "→ Final:", urspruenglich_abitur - abitur_und_buffer,
    sprintf("(%.1f%% behalten)\n", 100 * (urspruenglich_abitur - abitur_und_buffer) / urspruenglich_abitur))
cat("Nicht-Abitur ursprünglich:", urspruenglich_nicht_abitur, "→ Final:", urspruenglich_nicht_abitur - nicht_abitur_und_buffer,
    sprintf("(%.1f%% behalten)\n", 100 * (urspruenglich_nicht_abitur - nicht_abitur_und_buffer) / urspruenglich_nicht_abitur))

cat("\n=== FINALER DATENSATZ (haeuser_final) ===\n")
cat("Gesamt Häuser:", nrow(haeuser_final), "\n")
cat("Treatment Abitur:", sum(haeuser_final$treat_abitur), "\n")
cat("Treatment Nicht-Abitur:", sum(haeuser_final$treat_kein_abitur), "\n")
cat("Control:", sum(haeuser_final$treat_abitur == 0 & haeuser_final$treat_kein_abitur == 0), "\n")
