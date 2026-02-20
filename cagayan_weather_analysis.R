# ============================================
# PROJECT: Cagayan Valley Weather Analysis
# AUTHOR:  Angel V. Ole
# DATE:    February 2026
# DATA:    metar-taf.com (Temperature & Humidity)
#          Estimated daily rainfall (1-15mm/day)
# PURPOSE: IGP Crop Calendar — CSU-Piat COA
# ============================================

library(tidyverse)

set.seed(31)

# Real 2025 monthly means from metar-taf.com
monthly_temps <- rep(c(29.3, 30.5, 31.1, 31.3, 32.2, 31.5,
                       29.8, 30.6, 31.1, 32.2, 31.1, 31.1), each = 30)

monthly_humidity <- rep(c(75, 73, 76, 79, 81, 87,
                          91, 88, 87, 82, 80, 74), each = 30)

# Build the dataset
cagayan_weather <- data.frame(
  month = rep(1:12, each = 30),
  month_name = rep(month.abb, each = 30),
  temperature = round(rnorm(360, mean = monthly_temps, sd = 0.5), 1),
  rainfall = round(runif(360, min = 1, max = 15), 1),
  humidity = round(rnorm(360, mean = monthly_humidity, sd = 3), 1)
)

# Fix month_name order
cagayan_weather$month_name <- factor(cagayan_weather$month_name,
                                     levels = month.abb,
                                     ordered = TRUE)

head(cagayan_weather)

# Check for missing values
sum(is.na(cagayan_weather))

# Check structure of your data
str(cagayan_weather)

# Check for outliers - any temperature below 25 or above 40?
cagayan_weather %>%
  filter(temperature < 25 | temperature > 40)

# Check for impossible rainfall values (negative numbers)
cagayan_weather %>%
  filter(rainfall < 0)

# Check for impossible humidity values (outside 0-100%)
cagayan_weather %>%
  filter(humidity < 0 | humidity > 100)

# Summarize by month
monthly_summary <- cagayan_weather %>%
  group_by(month, month_name) %>%
  summarise(
    avg_temp = mean(temperature),
    total_rainfall = sum(rainfall),
    avg_humidity = mean(humidity),
    .groups = "drop"
  )

# View the summary
print(monthly_summary)

# Tag each month as GOOD, OKAY, or BAD for planting
monthly_summary <- monthly_summary %>%
  mutate(
    planting_condition = case_when(
      avg_temp >= 25 & avg_temp <= 35 &
        avg_humidity >= 60 & avg_humidity <= 80 ~ "GOOD",
      avg_humidity > 80 ~ "TOO WET",
      TRUE ~ "OKAY"
    )
  )
print(monthly_summary)

# LIMITATION NOTE:
# Rainfall was initially simulated using monthly min/max values (36-324mm)
# applied to daily rows, resulting in unrealistically high monthly totals.
# This was corrected by using realistic daily rainfall values (1-15mm/day)
# which better reflects actual daily precipitation in Cagayan Valley.
# Temperature and humidity data are based on real 2025 metar-taf.com records.

# Create crop calendar data
crop_calendar <- data.frame(
  crop = c("Gmelina", "Mango", "Sitaw", "Pechay", "Oyster Mushroom"),
  project = c("Plant Grow Repeat", "Plant Grow Repeat",
              "Halayaman", "Halayaman", "Kabutihan"),
  jan  = c("PLANT","PLANT","PLANT","PLANT","WAIT"),
  feb  = c("PLANT","PLANT","PLANT","PLANT","WAIT"),
  mar  = c("PLANT","PLANT","PLANT","PLANT","WAIT"),
  apr  = c("PLANT","PLANT","PLANT","PLANT","WAIT"),
  may  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  jun  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  jul  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  aug  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  sep  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  oct  = c("WAIT","WAIT","WAIT","WAIT","GROW"),
  nov  = c("PLANT","PLANT","PLANT","PLANT","WAIT"),
  dec  = c("PLANT","PLANT","PLANT","PLANT","WAIT")
)

# Reshape for ggplot
crop_long <- crop_calendar %>%
  pivot_longer(cols = jan:dec,
               names_to = "month",
               values_to = "status") %>%
  mutate(month = factor(month,
                        levels = c("jan","feb","mar","apr","may","jun",
                                   "jul","aug","sep","oct","nov","dec"),
                        ordered = TRUE))

install.packages("patchwork")
library(patchwork)

# CHART 1 - Temperature
p1 <- ggplot(monthly_summary, aes(x = month_name, y = avg_temp, fill = avg_temp)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Average Temperature",
       x = "", y = "°C",
       caption = "Source: metar-taf.com") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# CHART 2 - Humidity
p2 <- ggplot(monthly_summary, aes(x = month_name, y = avg_humidity, fill = avg_humidity)) +
  geom_col() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(title = "Average Humidity",
       x = "", y = "%",
       caption = "Source: metar-taf.com") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# CHART 3 - Plant Grow Repeat Calendar
pgr_data <- crop_long %>%
  filter(project == "Plant Grow Repeat")

p3 <- ggplot(pgr_data, aes(x = month, y = crop, fill = status)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(values = c("PLANT" = "#2ecc71", "WAIT" = "#e74c3c")) +
  labs(title = "Plant, Grow, Repeat",
       x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "#2c3e50"),
        strip.text = element_text(color = "white", face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# CHART 4 - Halayaman Calendar
hal_data <- crop_long %>%
  filter(project == "Halayaman")

p4 <- ggplot(hal_data, aes(x = month, y = crop, fill = status)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(values = c("PLANT" = "#2ecc71", "WAIT" = "#e74c3c")) +
  labs(title = "Halayaman",
       x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# CHART 5 - Kabutihan Calendar
kab_data <- crop_long %>%
  filter(project == "Kabutihan")

p5 <- ggplot(kab_data, aes(x = month, y = crop, fill = status)) +
  geom_tile(color = "white", linewidth = 0.8) +
  scale_fill_manual(values = c("GROW" = "#3498db", "WAIT" = "#e74c3c")) +
  labs(title = "Kabutihan",
       x = "", y = "", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7))

# COMBINE INTO DASHBOARD
dashboard <- (p1 | p2) /
  p3 /
  (p4 | p5) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9)
    )
  )

# DISPLAY DESIGN 1
dashboard

dashboard <- ((p1 | p2) /
                p3 /
                p4 /
                p5) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9)
    )
  )

# DISPLAY DESIGN 2
dashboard

# COMBINE INTO DASHBOARD - 2 column layout
dashboard <- ((p1 / p2) | (p3 / p4 / p5)) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.caption = element_text(size = 9)
    )
  )

# DISPLAY DESIGN 3
dashboard

# Save Design 1
dashboard1 <- ((p1 | p2) /
                 p3 /
                 (p4 | p5)) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 11),
                  plot.caption = element_text(size = 9)))

ggsave("dashboard_design1.png", plot = dashboard1, width = 16, height = 10, dpi = 300)

# Save Design 2
dashboard2 <- ((p1 | p2) / p3 / p4 / p5) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 11),
                  plot.caption = element_text(size = 9)))

ggsave("dashboard_design2.png", plot = dashboard2, width = 16, height = 12, dpi = 300)

# Save Design 3
dashboard3 <- ((p1 / p2) | (p3 / p4 / p5)) +
  plot_annotation(
    title = "Cagayan Valley 2025 Weather & Crop Calendar Dashboard",
    subtitle = "CSU-Piat College of Agriculture IGP — Based on Real 2025 Weather Data",
    caption = "Analysis by Angel V. Ole | Data: metar-taf.com",
    theme = theme(plot.title = element_text(size = 16, face = "bold"),
                  plot.subtitle = element_text(size = 11),
                  plot.caption = element_text(size = 9)))

ggsave("dashboard_design3.png", plot = dashboard3, width = 16, height = 10, dpi = 300)
