#Analyze pollinator dataset, when complete, add this script to main script
#load libraries
library(here)
library(tidyverse)
library(sf)
library(tigris)

#read all pollinator data csv file
pollinator_df <- read.csv("Pollinator Data CA filtered.csv", header = TRUE)

#summarize year counts
 pollinator_count <- pollinator_df %>% 
  group_by(year) %>% 
  summarise(total_count = sum(individualCount, na.rm = TRUE))
 
 ungroup(pollinator_df)

#filter for most robust data year
pollinator_df_filter <- pollinator_df %>% 
  filter(year == 2012)

#convert to spatial points
pollinator_sf <- st_as_sf(pollinator_df_filter,
                       coords = c("decimalLongitude", "decimalLatitude"),
                       crs = 4326)

#get CA county boundaries
ca_counties <- counties(state = "CA", cb = TRUE)

#ensure coordinate systems match
ca_counties <- st_transform(ca_counties, st_crs(pollinator_sf))

#assign each coordinate to a county
pollinator_with_county <- st_join(pollinator_sf, ca_counties)

#pollinators by county
pollinator_by_county <- pollinator_with_county %>% 
  group_by(NAME) %>% 
  summarise(total_pollinators = sum(individualCount, na.rm = TRUE))


