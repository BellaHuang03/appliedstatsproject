### set up
library(here)
library(tidyverse)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(janitor)
library(ggplot2)
library(patchwork)
library(ggpmisc)
library(tidyverse)
library(tigris)
library(sf)

### read datasets
pcp_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_pcp_2013_2023_clean.csv"))
tavg_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_tavg_2013_2023_clean.csv"))
final_clean <- read_csv("final_clean.csv")

### map out all counties in final_clean
options(tigris_use_cache = TRUE)

dataset_counties <- c(
  "Butte", "Colusa", "Contra Costa", "Fresno", "Glenn", "Kern", "Kings",
  "Madera", "Merced", "Napa", "Sacramento", "San Joaquin", "Shasta",
  "Solano", "Stanislaus", "Sutter", "Tehama", "Tulare", "Yolo", "Yuba"
)

ca_counties <- counties(state = "CA", cb = TRUE, year = 2022) %>%
  mutate(in_dataset = NAME %in% dataset_counties)

ggplot(ca_counties) +
  geom_sf(aes(fill = in_dataset), color = "white", linewidth = 0.4) +
  scale_fill_manual(
    values = c("TRUE" = "#1D9E75", "FALSE" = "#D3D1C7"),
    labels = c("TRUE" = "In dataset", "FALSE" = "Not in dataset"),
    name = NULL
  ) +
  labs(
    title = "California Counties in Tomato Yield Dataset",
    subtitle = "Processing tomatoes, September observations"
  ) +
  theme_void(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", margin = margin(b = 4)),
    plot.subtitle = element_text(color = "gray50", margin = margin(b = 12))
  ) 

#
# Install once if needed
# install.packages(c("tidyverse", "sf", "tigris", "ggplot2", "viridis", "stringr"))

library(tidyverse)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)
library(stringr)

options(tigris_use_cache = TRUE)

#
df <- read.csv("final_clean.csv")

# Inspect
glimpse(df)

# Expected columns based on your screenshot:
# County, year, month, temp, precip, yield, Data.Item

# Clean county names
df <- df %>%
  mutate(
    County = str_to_title(trimws(County))
  )

ca_counties <- counties(state = "CA", cb = TRUE, class = "sf") %>%
  select(NAME, geometry) %>%
  rename(County = NAME)

# Check county names
sort(unique(df$County))
sort(unique(ca_counties$County))

county_avg <- df %>%
  group_by(County) %>%
  summarise(
    mean_yield = mean(yield, na.rm = TRUE),
    mean_temp = mean(temp, na.rm = TRUE),
    mean_precip = mean(precip, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# Join to county polygons
map_avg <- ca_counties %>%
  left_join(county_avg, by = "County")

ggplot(map_avg) +
  geom_sf(aes(fill = mean_yield), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Average Tomato Yield by California County",
    subtitle = "Mean yield across all records in final dataset",
    fill = "Yield\n(tons/acre)"
  ) +
  theme_minimal()

# Compute county centroids for labels
county_labels <- st_centroid(map_avg)

ggplot(map_avg) +
  geom_sf(aes(fill = mean_yield), color = "white", linewidth = 0.2) +
  geom_sf_text(
    data = county_labels,
    aes(label = County),
    size = 2.5,
    check_overlap = TRUE
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Average Tomato Yield by California County",
    fill = "Yield\n(tons/acre)"
  ) +
  theme_minimal()
