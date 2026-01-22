#########################
##### preliminaries #####
#########################

### load necessary packages

library(tidyverse)

##########################################################################
##### 1. identification of treated grid-cells with secondary schools #####
##### Main specification #################################################


#### create treated-cell dataset for secondary schools (first order treatment)

treated_cells <- schools %>%
  rowwise() %>%
  reframe(
    school_ID = school_ID,
    expand.grid(dx = -2:2, dy = -2:2) %>%
      mutate(dist = pmax(abs(dx), abs(dy))) %>%
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

#### match datasets using grid-identifyer

all_cells <- bind_rows(
  treated_once %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L,
      control  = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag),
      school_type_tag = as.character(school_type_tag)
    ),
  control_cells %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,
      control = 1L,
      source = "control_cells",
      school_tag = NA_character_,
    )
) %>%
  group_by(ergg_1km, x, y) %>%
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

#### use dataset to code final treatment-variable 
# if cell is treated = 1
# if cell is control = 0


all_cells <- all_cells %>%
  mutate(
    school_nearby = case_when(
      treated == 1 & control == 0 ~ 1L,
      treated == 1 & control == 1 ~ 1L,
      treated == 0 & control == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
  )  



##############################################################################
##### 2. Create second treatment-variable if the schools offers "Abitur" #####
###### Full specificatin including interaction term ##########################


##### code Variable "abitur_nearby" (second order treatment)

all_cells <- all_cells %>%
  mutate(school_type_code = sub("t$", "", school_type))

all_cells <- all_cells %>%
  mutate(abitur_nearby = if_else(school_type_code %in% c(15, 20), 1L, 0L)) # considers cell as treated, if it is within x of a school offering academic track

all_cells <- all_cells %>%
  mutate(school_tag_code = sub("t$", "", school_tag)) # separates school_ID from treated indicator for importing school data


#### import school data

all_cells_school <- all_cells %>%
  left_join(
    schools %>%
      mutate(school_ID = as.character(school_ID)) %>%
      select(school_ID, ssi),
    by = c("school_tag_code" = "school_ID")
  )


#### match information with housing data

full_dataset_main <- housing_data_NRW %>%
  left_join(all_cells_school, by = "ergg_1km")


#### exclude houses not lying in treated or control cells


full_dataset_main_clean <- full_dataset_main %>%
  filter(
    !if_all(
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


#####################################################
#### Treatment-Assignment for robustness checks #####
#####################################################
# here, we exclude "buffer-cells" with the distance of 2 around the treated-cells



#### create treated-cell dataset for secondary schools (first order treatment)

treated_cells_robustness <- schools %>%
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

treated_cells_robustness <- treated_cells_robustness %>%
  mutate(ergg_1km = paste0(x, "_", y))

### remove all multiple treated cells

treated_once_robustness <- treated_cells_robustness %>%
  group_by(ergg_1km)%>%
  filter(n() == 1) %>%
  ungroup()

#### create buffer-cells for the exclusion

buffer_cells_robustness <- schools %>%
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

buffer_cells_robustness <- buffer_cells_robustness %>%
  mutate(ergg_1km = paste0(x, "_", y))

#### check for overlaps

any(treated_once_robustness$ergg_1km %in% buffer_cells_robustness$ergg_1km)

overlap_ids <- intersect(
  treated_cells_robustness$ergg_1km,
  buffer_cells_robustness$ergg_1km
)

length(overlap_ids)


#### create non-treated cells

control_cells_robustness <- schools %>%
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

control_cells_robustness <- control_cells_robustness %>%
  mutate(ergg_1km = paste0(x, "_", y))

#### match datasets using grid-identifyer

all_cells_robustness <- bind_rows(
  treated_once_robustness %>%
    transmute(
      ergg_1km, x, y,
      treated = 1L,
      buffer  = 0L,
      control = 0L,
      source  = "treated_cells",
      school_tag = as.character(school_tag),
      school_type_tag = as.character(school_type_tag)
    ),
  buffer_cells_robustness %>%
    transmute(
      ergg_1km, x, y,
      treated = 0L,
      buffer  = 1L,
      control = 0L,
      source  = "buffer_cells",
      school_tag = NA_character_,
      school_type = NA_character_,
    ),
  control_cells_robustness %>%
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


#### create cell data-set without overlaps of treated/controls with buffer

all_cells_robustness_clean <- all_cells_robustness %>%
  filter(buffer != 1)



#### code final treatment-variable for robustness check

all_cells_robustness_clean <- all_cells_robustness_clean %>%
  mutate(
    school_nearby = case_when(
      treated == 1 & control == 0 ~ 1L,
      treated == 1 & control == 1 ~ 1L,
      treated == 0 & control == 1 ~ 0L,
      TRUE ~ NA_integer_
    )
  ) 


##### code second order treatment

all_cells_robustness_clean <- all_cells_robustness_clean %>%
  mutate(school_type_code = sub("t$", "", school_type))

all_cells_robustness_clean <- all_cells_robustness_clean %>%
  mutate(abitur_nearby = if_else(school_type_code %in% c(15, 20), 1L, 0L))

all_cells_robustness_clean <- all_cells_robustness_clean %>%
  mutate(school_tag_code = sub("t$", "", school_tag))

#### import school data

all_cells_school_robustness <- all_cells_robustness_clean %>%
  left_join(
    schools %>%
      mutate(school_ID = as.character(school_ID)) %>%
      select(school_ID, ssi),
    by = c("school_tag_code" = "school_ID")
  )


#### match information with housing data

full_dataset_robustness <- housing_data_NRW %>%
  left_join(all_cells_school_robustness, by = "ergg_1km")



#### exclude houses not lying in treated or control cells


full_dataset_robustness_clean <- full_dataset_robustness %>%
  filter(
    !if_all(
      c(
        treated,
        buffer,
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



full_dataset_robustness_clean %>%
  count(school_nearby)



