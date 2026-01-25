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
    )
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


# balance table                                          # ANPASSEN!
balance_table <- analysis_data %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_price_sqm = mean(price_sqm, na.rm = TRUE),
    sd_price_sqm = sd(price_sqm, na.rm = TRUE),
    mean_living_area = mean(wohnflaeche, na.rm = TRUE),
    sd_living_area = sd(wohnflaeche, na.rm = TRUE),
    mean_rooms = mean(zimmeranzahl, na.rm = TRUE),
    sd_rooms = sd(zimmeranzahl, na.rm = TRUE),
    mean_construction_year = mean(baujahr, na.rm = TRUE),
    sd_construction_year = sd(baujahr, na.rm = TRUE),
    .groups = "drop"
  )

stargazer(as.data.frame(balance_table),
          type = "latex",
          summary = FALSE,
          title = "house characteristics by treatment status",
          digits = 2,
          rownames = FALSE)


# t-tests                                                     # NOCHMAL PRÜFEN
# abitur vs. control
test_price_abitur_control <- t.test(
  price_sqm ~ abitur_nearby,
  data = analysis_data %>% filter(group %in% c("abitur", "control"))
)

# non-abitur vs. control
test_price_non_abitur_control <- t.test(
  price_sqm ~ non_abitur_nearby,
  data = analysis_data %>% filter(group %in% c("non_abitur", "control"))
)

# abitur vs. non-abitur
abitur_data <- analysis_data %>% filter(group == "abitur") %>% pull(price_sqm)
non_abitur_data <- analysis_data %>% filter(group == "non_abitur") %>% pull(price_sqm)
test_price_abitur_non_abitur <- t.test(abitur_data, non_abitur_data)

# create t-test results table
ttest_results <- data.frame(
  comparison = c("abitur vs. control", 
                 "non-abitur vs. control", 
                 "abitur vs. non-abitur"),
  difference = c(
    test_price_abitur_control$estimate[1] - test_price_abitur_control$estimate[2],
    test_price_non_abitur_control$estimate[1] - test_price_non_abitur_control$estimate[2],
    test_price_abitur_non_abitur$estimate[1] - test_price_abitur_non_abitur$estimate[2]
  ),
  t_statistic = c(
    test_price_abitur_control$statistic,
    test_price_non_abitur_control$statistic,
    test_price_abitur_non_abitur$statistic
  ),
  p_value = c(
    test_price_abitur_control$p.value,
    test_price_non_abitur_control$p.value,
    test_price_abitur_non_abitur$p.value
  )
)

stargazer(ttest_results,
          type = "latex",
          summary = FALSE,
          title = "t-tests: price differences",
          digits = 3,
          rownames = FALSE)


# plots                                                    # Y-ACHSE ANPASSEN!
plot_distribution <- analysis_data %>%
  filter(!is.na(group), price_sqm < 10000) %>%
  ggplot(aes(x = price_sqm, fill = group)) +
  geom_histogram(bins = 50, alpha = 0.7, position = "identity") +
  facet_wrap(~ group, ncol = 1) +
  scale_fill_manual(values = c("abitur" = "#2E86AB", 
                               "non_abitur" = "#A23B72", 
                               "control" = "#95B46A")) +
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
  scale_fill_manual(values = c("abitur" = "#2E86AB", 
                               "non_abitur" = "#A23B72", 
                               "control" = "#95B46A")) +
  labs(
    title = "house prices by treatment group",
    x = "group",
    y = "price per sqm (EUR)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
plot_boxplot
ggsave("plot_boxplot.pdf", plot_boxplot, width = 8, height = 6)
