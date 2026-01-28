library(tidyverse)
library(ggplot2)
library(stargazer)

# temporary dataset for analysis
analysis_data <- full_dataset_main_clean %>%
  mutate(
    non_abitur_nearby = if_else(school_nearby == 1 & abitur_nearby == 0, 1L, 0L),
    group = case_when(
      abitur_nearby == 1 ~ "abitur",
      school_nearby == 1 & abitur_nearby == 0 ~ "non_abitur",
      school_nearby == 0 ~ "control",
      TRUE ~ NA_character_
    ),
    # set factor order: abitur, non_abitur, control
    group = factor(group, levels = c("abitur", "non_abitur", "control"))
  )


# summary house characteristics
summary_stats <- analysis_data %>%
  filter(!is.na(group)) %>%
  select(price_sqm, wohnflaeche, zimmeranzahl, baujahr, group)

stargazer(as.data.frame(summary_stats %>% select(-group)),
          type = "latex",
          title = "summary statistics",
          summary = TRUE,
          digits = 2)


# price table by group 
price_table <- analysis_data %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean = mean(price_sqm, na.rm = TRUE),
    sd = sd(price_sqm, na.rm = TRUE),
    median = median(price_sqm, na.rm = TRUE),
    min = min(price_sqm, na.rm = TRUE),
    max = max(price_sqm, na.rm = TRUE),
    .groups = "drop"
  )

stargazer(as.data.frame(price_table),
          type = "latex",
          summary = FALSE,
          title = "average prices by group",
          digits = 2,
          rownames = FALSE)


# balance table 
balance_table <- analysis_data %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    price_mean = mean(price_sqm, na.rm = TRUE),
    price_sd = sd(price_sqm, na.rm = TRUE),
    area_mean = mean(as.numeric(wohnflaeche), na.rm = TRUE),
    area_sd = sd(as.numeric(wohnflaeche), na.rm = TRUE),
    rooms_mean = mean(as.numeric(zimmeranzahl), na.rm = TRUE),
    rooms_sd = sd(as.numeric(zimmeranzahl), na.rm = TRUE),
    year_mean = mean(as.numeric(baujahr), na.rm = TRUE),
    year_sd = sd(as.numeric(baujahr), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = -group, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = group, values_from = value)

stargazer(as.data.frame(balance_table),
          type = "latex",
          summary = FALSE,
          title = "house characteristics by treatment status",
          digits = 2,
          rownames = FALSE)


# balance tests (significance tests)
vars <- c("price_sqm", "wohnflaeche", "zimmeranzahl", "baujahr")

# abitur vs. control
balance_tests_1 <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ abitur_nearby")), 
                 data = analysis_data %>% filter(group %in% c("abitur", "control")))
  data.frame(
    comparison = "abitur vs. control",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# non_abitur vs. control
balance_tests_2 <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ non_abitur_nearby")), 
                 data = analysis_data %>% filter(group %in% c("non_abitur", "control")))
  data.frame(
    comparison = "non_abitur vs. control",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# abitur vs. non_abitur
balance_tests_3 <- lapply(vars, function(v) {
  data_abitur <- analysis_data %>% filter(group == "abitur") %>% pull(!!sym(v))
  data_non_abitur <- analysis_data %>% filter(group == "non_abitur") %>% pull(!!sym(v))
  test <- t.test(data_abitur, data_non_abitur)
  data.frame(
    comparison = "abitur vs. non_abitur",
    variable = v,
    mean_diff = test$estimate[1] - test$estimate[2],
    t_stat = test$statistic,
    p_value = test$p.value
  )
}) %>% bind_rows()

# combine all tests and format p-values
balance_tests <- bind_rows(balance_tests_1, balance_tests_2, balance_tests_3) %>%
  mutate(
    mean_diff = round(mean_diff, 3),
    t_stat = round(t_stat, 3),
    p_value = sprintf("%.3f", p_value)  # Format p-values with exactly 3 decimals
  ) %>%
  select(comparison, variable, mean_diff, t_stat, p_value)

stargazer(as.data.frame(balance_tests),
          type = "latex",
          summary = FALSE,
          title = "balance tests: significance of differences",
          digits = 3,
          no.space = TRUE,
          rownames = FALSE)


# balance test simple
vars <- c("price_sqm", "wohnflaeche", "zimmeranzahl", "baujahr")

# school_nearby vs. control
balance_tests <- lapply(vars, function(v) {
  test <- t.test(as.formula(paste(v, "~ school_nearby")), 
                 data = analysis_data %>% filter(school_nearby %in% c(0, 1)))
  data.frame(
    comparison = "school_nearby vs. control",
    variable = v,
    mean_diff = round(test$estimate[1] - test$estimate[2], 3),
    t_stat = round(test$statistic, 3),
    p_value = sprintf("%.3f", test$p.value)
  )
}) %>% bind_rows() %>%
  select(comparison, variable, mean_diff, t_stat, p_value)

stargazer(as.data.frame(balance_tests),
          type = "latex",
          summary = FALSE,
          title = "balance tests: school_nearby vs. control",
          digits = 3,
          no.space = TRUE,
          rownames = FALSE)


# plots
plot_distribution <- analysis_data %>%
  filter(!is.na(group), price_sqm < 10000) %>%
  ggplot(aes(x = price_sqm, fill = group)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~ group, ncol = 1, scales = "free_y") +  
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "distribution of house prices by treatment status",
    x = "price per sqm (EUR)",
    y = "number of houses"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
plot_distribution
ggsave("plot_distribution.pdf", plot_distribution, width = 8, height = 10)


plot_boxplot <- analysis_data %>%
  filter(!is.na(group), price_sqm < 10000) %>%
  ggplot(aes(x = group, y = price_sqm, fill = group)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Accent") +
  labs(
    title = "house prices by treatment group",
    x = "group",
    y = "price per sqm (EUR)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
plot_boxplot
ggsave("plot_boxplot.pdf", plot_boxplot, width = 8, height = 6)