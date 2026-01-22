library(ggplot2)
library(dplyr)

# create grid and assign zones (with exemplary coordinates)

x_start <- 4011
y_start <- 4231

grid <- expand.grid(x = x_start:(x_start + 10), 
                    y = y_start:(y_start + 10)) %>%
  mutate(
    dist = pmax(abs(x - (x_start + 5)), abs(y - (y_start + 5))),
    zone = case_when(
      dist <= 1 ~ "Treatment-Zone",
      dist == 2 ~ "Buffer-Zone",
      dist %in% 3:4 ~ "Control-Zone",
      TRUE ~ "Untreated"
    ),
    zone = factor(zone, levels = c("Treatment-Zone", "Buffer-Zone", 
                                   "Control-Zone", "Untreated"))
  )

# plot
ggplot(grid, aes(x, y, fill = zone)) +
  geom_tile(color = "black", linewidth = 0.5) +
  annotate("text", x = x_start + 5, y = y_start + 5, 
           label = "X", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c("Treatment-Zone" = "#B3D9FF", "Buffer-Zone" = "#FFFF66",
               "Control-Zone" = "#90EE90", "Untreated" = "#E8F5E8"),
    name = "Legend:"
  ) +
  coord_fixed(expand = FALSE) +
  scale_x_continuous(breaks = seq(x_start, x_start + 10, by = 2), 
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(y_start, y_start + 10, by = 2), 
                     expand = c(0, 0)) +
  labs(x = "X-Coordinate", y = "Y-Coordinate",
       title = "School Treatment Zones (Queen Contiguity)",
       subtitle = "Each cell represents 1 km²") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold"))
