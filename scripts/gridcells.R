#########################
##### preliminaries #####
#########################

### load necessary packages

library(tidyverse)
library(readxl)

#### import data

schools <- read_xlsx("school_data.xlsx")
housing_data <- read_csv("~/Uni/Data Analysis Using R/CampusFile_HK_2022.csv")
ssi_data <- read_csv2("2022_social_index.csv")

############################
##### prepare datasets #####
############################

#### create full school dataset

ssi_data <- ssi_data %>%
  rename(school_ID = Schulnummer,
         ssi = Sozialindexstufe)

schools <- schools %>%
  left_join(
    ssi_data %>% 
      select(school_ID, ssi),
    by = "school_ID"
  ) 

#### clean-up housing data 
## remove NA's on grid

housing_data <- housing_data %>%
  filter(ergg_1km != -9)

################################################
##### identification of treated grid-cells #####
################################################

#### include only neccessary school types

schools <- schools %>%
  filter(school_type %in% c(4, 10, 14, 15, 20))

#### separate grid-id

schools <- schools %>%
  separate(ergg_1km, c("x", "y"), "_", remove = FALSE, convert = TRUE)

#### create treated-cell dataset for 'Abitur'-schools

treated_cells <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1)) %>%
      transmute(
        x = x + dx,
        y = y + dy,
        school_tag = if_else(dx == 0 & dy == 0,
                             as.character(school_ID),
                             paste0(school_ID, "t")),
        school_type_tag = if_else(dx == 0 & dy == 0,
                                  as.character(school_type),
                                  paste0(school_type, ("t")))
      )
  ) %>%
  ungroup()

treated_cells <- treated_cells %>%
  mutate(ergg_1km = paste0(x, "_", y))

### remove all multiple treated cells

treated_once <- treated_cells %>%
  group_by(ergg_1km)%>%
  filter(n() == 1) %>%
  ungroup()

#### create buffer-cells for the exclusion

buffer_cells <- schools %>%
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

any(treated_once$ergg_1km %in% buffer_cells$ergg_1km)

overlap_ids <- intersect(
  treated_cells$ergg_1km,
  buffer_cells$ergg_1km
)

length(overlap_ids)

#### match datasets using grid-identifyer

all_cells <- bind_rows(
  treated_once %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L,
      buffer  = 0L,
      control = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag),
      school_type_tag = as.character(school_type_tag)
    ),
  buffer_cells %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,
      buffer  = 1L,
      control = 0L,
      source  = "buffer_cells",
      school_tag = NA_character_,
      school_type = NA_character_,
    ),
  control_cells %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,
      buffer  = 0L,
      control = 1L,
      source = "control_cells",
      school_tag = NA_character_,
    )
) %>%
  group_by(ergg_1km, x, y) %>%
  summarise(
    treated = max(treated),
    buffer  = max(buffer),
    control = max(control),
    source  = paste(sort(unique(source)), collapse = " + "),
    school_tag = if (all(is.na(school_tag))) NA_character_
    else paste(sort(unique(na.omit(school_tag))), collapse = ", "),
    school_type = if (all(is.na(school_type_tag))) NA_character_
                  else paste(sort(unique(na.omit(school_type_tag))), collapse = ", "),
    .groups = "drop"
  )

##### code second order treatment

all_cells <- all_cells %>%
  mutate(school_type_code = sub("t$", "", school_type))

all_cells <- all_cells %>%
  mutate(abitur_nearby = if_else(school_type_code %in% c(15, 20), 1L, 0L))

all_cells <- all_cells %>%
  mutate(school_tag_code = sub("t$", "", school_tag))

#### import school data

all_cells_school <- all_cells %>%
  left_join(
    schools %>%
      mutate(school_ID = as.character(school_ID)) %>%
      select(school_ID, ssi),
    by = c("school_tag_code" = "school_ID")
  )

#### create non-treated cells

control_cells <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = -4:4, dy = -4:4) %>%
      mutate(dist = pmax(abs(dx), abs(dy))) %>%
      filter(dist %in% c(3, 4)) %>%
      transmute(
        x = x + dx,
        y = y + dy,
        distanz = dist,
        school_tag = paste0(school_ID, "c")
      )
  ) %>%
  ungroup() %>%
  distinct()

control_cells <- control_cells %>%
  mutate(ergg_1km = paste0(x, "_", y))

#### match information with housing data

full_dataset <- housing_data %>%
  left_join(all_cells_school, by = "ergg_1km")

#### create final treated data-set without overlaps 

full_dataset_clean <- full_dataset %>%
  filter(buffer != 1)
