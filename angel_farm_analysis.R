# ================================================
# PROJECT 1: Angel Farm Agricultural Analysis
# Author: Angel Ole
# Date: February 18, 2026
# Tool: R 4.5.2 | Posit Cloud
# Goal: Explore and visualize farm data using R
# ================================================

# --- LOAD LIBRARIES ---
library(tidyverse)

# --- CREATE DATASET ---
angel_farm <- data.frame(
  province = c("Cagayan", "Isabela", "Kalinga",
               "Cagayan", "Isabela", "Kalinga",
               "Cagayan", "Isabela", "Kalinga"),
  animals = c("Chicken", "Chicken", "Chicken",
              "Pig", "Pig", "Pig",
              "Cow", "Cow", "Cow"),
  population = c(9000, 10000, 20000,
                 300, 500, 600,
                 100, 700, 300),
  year = rep(2028, 9)
)

# --- EXPLORE DATA ---
str(angel_farm)
summary(angel_farm)
nrow(angel_farm)
ncol(angel_farm)
colnames(angel_farm)

# --- FILTER & SORT ---

# LARGEST POPULATION: Sort by population (highest to lowest)
largest_population <- angel_farm %>%
  arrange(desc(population))
print(largest_population)

# FILTER CHICKEN:
chicken_only <- largest_population %>%
  filter(animals == "Chicken")
print(chicken_only)

# FILTER PIG:
pig_only <- largest_population %>%
  filter(animals == "Pig")
print(pig_only)

# FILTER COW:
cow_only <- largest_population %>%
  filter(animals == "Cow")
print(cow_only)

# AVERAGE:
avg_population <- angel_farm %>%
  group_by(animals) %>%
  summarise(average = mean(population))
print(avg_population)

# SUMMARY STATISTICS:
summary_stats <- angel_farm %>%
  group_by(animals) %>%
  summarise(
    average  = mean(population),
    highest  = max(population),
    lowest   = min(population),
    total    = sum(population),
    best_performing_province = province[which.max(population)]
  )
print(summary_stats)

# --- VISUALIZATION ---
ggplot(angel_farm, aes(x = province, y = population, fill = animals)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title    = "Angel's Farm Population by Province",
    subtitle = "Animal population across Cagayan, Isabela, and Kalinga - 2028",
    x        = "Province",
    y        = "Population Count",
    fill     = "Animal Type"
  ) +
  theme_minimal() +
  theme(
    plot.title        = element_text(size = 16, face = "bold", color = "#2C5F2D"),
    plot.subtitle     = element_text(size = 10, color = "#666666"),
    axis.title        = element_text(size = 11, face = "bold"),
    legend.position   = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor  = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Chicken" = "#F4A620",
    "Cow"     = "#2C5F2D",
    "Pig"     = "#97BC62"
  ))
