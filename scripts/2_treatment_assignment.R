#########################
##### preliminaries #####
#########################

### load necessary packages

library(tidyverse)

##########################################################################
##### 1. identification of treated grid-cells with secondary schools #####
##### Main specification #################################################


#### create treated-cell data-set for secondary schools (first order treatment)

treated_cells <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = -1:1, dy = -1:1) %>%   # treated cells within a distance of up to one around the school-cell
      mutate(dist = pmax(abs(dx), abs(dy))) %>%
      transmute(
        x = x + dx,
        y = y + dy,
        school_tag = if_else(dx == 0 & dy == 0,  # keep relevant school information for each treated cell
                             as.character(school_ID),
                             paste0(school_ID, "t")),
        school_type_tag = if_else(dx == 0 & dy == 0,
                                  as.character(school_type),
                                  paste0(school_type, ("t")))
      )
  ) %>%
  ungroup()

treated_cells <- treated_cells %>%  # reassemble grid-id
  mutate(ergg_1km = paste0(x, "_", y))

### remove all multiple treated cells

treated_once <- treated_cells %>%
  group_by(ergg_1km)%>%
  filter(n() == 1) %>%
  ungroup()

#### create non-treated cells

control_cells <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = -4:4, dy = -4:4) %>%
      mutate(dist = pmax(abs(dx), abs(dy))) %>%
      filter(dist %in% c(2, 3, 4)) %>%   # cells within a distance of 2-4 around the school cell are consiered controls
      transmute(
        x = x + dx,
        y = y + dy,
        distanz = dist,
        school_tag = paste0(school_ID, "c")
      )
  ) %>%
  ungroup() %>%
  distinct()

control_cells <- control_cells %>%  # reassemble grid-id
  mutate(ergg_1km = paste0(x, "_", y))

#### match identified cells for each category in one data-set using grid-identifyer

all_cells <- bind_rows(
  treated_once %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L,  # auxiliary variables to identify overlaps
      control  = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag),
      school_type_tag = as.character(school_type_tag)
    ),
  control_cells %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,  # auxiliary variables to identify overlaps
      control = 1L,
      source = "control_cells",
      school_tag = NA_character_,
    )
) %>%
  group_by(ergg_1km, x, y) %>%  # organize nessecary variables per grid-cell on school and treatment status
  summarise(
    treated = max(treated),
    control = max(control),
    source  = paste(sort(unique(source)), collapse = " + "),
    school_tag = if (all(is.na(school_tag))) NA_character_
    else paste(sort(unique(na.omit(school_tag))), collapse = ", "),
    school_type = if (all(is.na(school_type_tag))) NA_character_
    else paste(sort(unique(na.omit(school_type_tag))), collapse = ", "),
    .groups = "drop"
  )

#### use data-set to code final treatment-variable 
# if cell is treated = 1
# if cell is control = 0


all_cells <- all_cells %>%
  mutate(
    school_nearby = case_when(
      treated == 1 & control == 0 ~ 1L, # unambiguous treated cells 
      treated == 1 & control == 1 ~ 1L, # keep double coded cells for treatment and control as treated
      treated == 0 & control == 1 ~ 0L, # control cells
      TRUE ~ NA_integer_
    )
  )  



##############################################################################
##### 2. Create second treatment-variable if the schools offers "Abitur" #####
###### Full specificatin including interaction term ##########################


##### code Variable "abitur_nearby" (second order treatment) for interaction

all_cells <- all_cells %>% # generate school ID from auxiliary variable for all cells
  mutate(school_type_code = sub("t$", "", school_type))

all_cells <- all_cells %>% 
  mutate(abitur_nearby = if_else(school_type_code %in% c(15, 20), 1L, 0L)) # considers cell as treated, if it is within x of a school offering academic track

all_cells <- all_cells %>%
  mutate(school_tag_code = sub("t$", "", school_tag)) # separates school_ID from treated indicator for importing school data


#### import school data

all_cells_school <- all_cells %>%
  left_join(
    schools %>%
      mutate(school_ID = as.character(school_ID)) %>% # gets information on SSI for each cell
      select(school_ID, ssi),
    by = c("school_tag_code" = "school_ID")
  )


#### match information with housing data

full_dataset_main <- housing_data_NRW %>%   # attach information on treatment status and schools on housing data
  left_join(all_cells_school, by = "ergg_1km")


#### exclude houses not lying in treated or control cells


full_dataset_main_clean <- full_dataset_main %>%
  filter(
    !if_all( # we know that every house in the data-set that has no information on the variables is not located in any relevant cell for the analysis
      c(
        treated,
        control,
        source,
        school_tag,
        school_type,
        school_nearby,
        school_type_code,
        abitur_nearby,
        school_tag_code
      ),
      is.na
    )
  )

#### check N

full_dataset_main_clean %>%
  count(school_nearby)
