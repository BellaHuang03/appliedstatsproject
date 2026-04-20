#load libraries
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
library(nlme)
library(tidyverse)
library(tigris)
library(sf)

# README: Our group put the codes of our complicated data scraping and cleaning at the end of this file, with the codes that we used for initial figures.

### IMPORT DATASETS
### read datasets
pcp_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_pcp_2013_2023_clean.csv"))
tavg_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_tavg_2013_2023_clean.csv"))
final_clean <- read_csv("final_clean.csv")

###############################################################################
### Initial data treatment and exploration
# Add columns for transformed variables
# Log transform precipitation variable
# Add temperature squared 
final_clean <- final_clean %>% 
  mutate(precip_add_1 = precip + 1) %>% 
  mutate(precip_log = log(precip_add_1)) %>% 
  mutate(temp_squared = temp^2)

# data structure exploring
str(final_clean)
summary(final_clean)
head(final_clean)

# How many observations per county?
final_clean %>%
  count(County)

# Distribution of temperature
p_temp_dist <- ggplot(final_clean, aes(x = temp)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Temperature from 2013-2023 per month",
       x = "Temperature (°F)", y = "Count") +
  theme_bw()
p_temp_dist

# Distribution of precipitation
p_precip_dist <- ggplot(final_clean, aes(x = precip)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Precipitation from 2013-2023 per month",
       x = "Precipitation (in)", y = "Count") +
  theme_bw()
p_precip_dist

# Distribution of log transformed precipitation
p_precip_log_dist <- ggplot(final_clean, aes(x = precip_log)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Precipitation",
       x = "Precipitation (in)", y = "Count") +
  theme_bw()
p_precip_log_dist

# Distribution of yield (response variable)
p_yield_dist <- ggplot(final_clean, aes(x = yield)) +
  geom_histogram(bins = 20, fill = "forestgreen", color = "white") +
  labs(title = "Distribution of Yield in TONS / ACRE",
       x = "Yield", y = "Count") +
  theme_bw()
p_yield_dist

# Count of observations per county (categorical)
p_county_count <- ggplot(final_clean, aes(x = fct_infreq(County))) +
  geom_bar(fill = "plum4") +
  labs(title = "Observations per County",
       x = "County", y = "Count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Patch all distribution plots together
(p_temp_dist | p_precip_dist) / (p_yield_dist | p_county_count)

## relationship between variables
# Yield ~ Temperature
p_yield_temp <- ggplot(final_clean, aes(x = temp, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(title = "Yield vs. Temperature",
       x = "Temperature (°F)", y = "Yield") +
  theme_bw()

p_yield_temp

# Yield ~ Precipitation
p_yield_precip <- ggplot(final_clean, aes(x = precip, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Yield vs. Precipitation",
       x = "Precipitation (in)", y = "Yield") +
  theme_bw()
p_yield_precip

# Yield ~ County (boxplot, colored by county)
p_yield_county <- ggplot(final_clean, aes(x = County, y = yield)) +
  geom_boxplot(width = 0.4, outlier.shape = NA) +
  geom_jitter(aes(color = County), width = 0.15, alpha = 0.6, size = 2) +
  labs(title = "Yield by County",
       x = "County", y = "Yield") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Yield ~ Year (to look for temporal trends)
p_yield_year <- ggplot(final_clean, aes(x = year, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "forestgreen") +
  labs(title = "Yield vs. Year",
       x = "Year", y = "Yield") +
  theme_bw()
p_yield_year

# Patch relationship plots together
(p_yield_temp | p_yield_precip) / (p_yield_county | p_yield_year)

## Correlation check
# Temp vs. Precip
p_temp_precip <- ggplot(final_clean, aes(x = temp, y = precip)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40") +
  labs(title = "Temperature vs. Precipitation",
       x = "Temperature (°F)", y = "Precipitation (in)") +
  theme_bw()

p_temp_precip

# Pearson correlation coefficient
cor(final_clean$temp, final_clean$precip, use = "complete.obs")

# Correlation matrix across all numeric variables
final_clean %>%
  select(temp, precip, yield, year) %>%
  cor(use = "complete.obs")

p_yield_county_time <- ggplot(final_clean, aes(x = year, y = yield, color = County)) +
  geom_line() +
  geom_point(size = 2) +
  labs(title = "Yield Over Time by County",
       x = "Year", y = "Yield") +
  theme_bw() +
  theme(legend.position = "right")

p_yield_county_time

###############################################################################
### Further data exploration - county level variation
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

options(tigris_use_cache = TRUE)

df <- read.csv("final_clean.csv")
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

###############################################################################
# Model 1: simple linear regression for temp and precip
# yield ~ precip
model_precip <- lm(yield ~ precip, data = final_clean)
summary(model_precip)

# yield ~ temp
model_temp <- lm(yield ~ temp, data = final_clean)
summary(model_temp)

###############################################################################
## Model 2: Multivariate linear model
model_both <- lm(yield ~ temp + precip, data = final_clean)
summary(model_both)

# Residual diagnostic plots (4 plots)
par(mfrow = c(2, 2))
plot(model_both)
par(mfrow = c(1, 1))

# Predicted vs. Actual
final_clean$predicted <- predict(model_both)

p_pred_actual <- ggplot(final_clean, aes(x = predicted, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Predicted vs. Actual Yield",
       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
  theme_bw()

p_pred_actual

###############################################################################
## Model 3: Multivariate linear model with temp squared AND precip logged
lm_temp2_preclog <- lm(yield ~ temp_squared + precip_log, data = final_clean)
summary(lm_temp2_preclog)

# Residual diagnostic plots (4 plots)
par(mfrow = c(2, 2))
plot(lm_temp2_preclog)
par(mfrow = c(1, 1))

# Predicted vs. Actual
final_clean$predicted_temp2_preclog <- predict(lm_temp2_preclog)

p_pred_actual_temp2 <- ggplot(final_clean, aes(x = predicted_temp2_preclog, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Predicted vs. Actual Yield",
       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
  theme_bw()

p_pred_actual_temp2

###############################################################################
# Model 4: Random Effects Model - harvest season
# Make counties into factors
final_clean <- final_clean %>% 
  mutate(County = factor(County, levels = c("BUTTE", "COLUSA", "CONTRA COSTA", "FRESNO", "GLENN", 
                                            "IMPERIAL", "KERN", "KINGS", "MADERA", "MERCED", "MONTEREY", 
                                            "SACRAMENTO", "SAN BENITO", "SAN JOAQUIN", "SANTA CLARA", 
                                            "SOLANO", "STANISLAUS", "SUTTER", "TEHAMA", "TULARE", 
                                            "YOLO", "YUBA")))

# Run random effects model with temp squared AND precip logged
simple_temp2 <- gls(yield ~ temp_squared, data = final_clean)
simple_precip_log <- gls(yield ~ precip_log, data = final_clean)
linearmodel <- gls(yield ~ temp_squared + precip_log, data = final_clean)
re_model <- lme(yield ~ temp_squared + precip_log, 
                random = ~1|County, data = final_clean)
summary(re_model)

# Residual diagnostic plots (2 plots)
plot(re_model)
qqnorm(re_model)

# Compare models with AIC
AIC(simple_temp2, simple_precip_log, linearmodel, re_model)

# Predicted vs. Actual
final_clean$predicted_re <- predict(re_model)

re_fig <- ggplot(final_clean, aes(x = predicted_re, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Predicted vs. Actual Yield",
       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
  theme_bw()

re_fig

## AIC calculation
AIC(simple_temp2, simple_precip_log, linearmodel, re_model)

###############################################################################
### Growing Season Analysis
# Segmented temp and precip by average temp between march- June of tomato 
# growing season 

season_clean <- read_csv("season_final.csv")


seasontemp_dist <- ggplot(season_clean, aes(x = tavg_mean)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Average Temperature in Growing Season from 2013-2023",
       x = "Temperature (°F)", y = "Count") +
  theme_bw()
seasontemp_dist 

# Distribution of precipitation
seasonprecip_dist <- ggplot(season_clean, aes(x = pcp_mean)) +
  geom_histogram(bins = 20, fill = "darkorange", color = "white") +
  labs(title = "Distribution of Average Precipitation in Growing Season from 2013-2023",
       x = "Precipitation (in)", y = "Count") +
  theme_bw()

seasonprecip_dist


#Log transform sesaonal precipitation data
season_cleanlog <- season_clean %>% 
  mutate(precip_meanlog = log(pcp_mean)) 

# Distribution of log transformed precipitation

seasonlogprecip_dist <- ggplot(season_cleanlog, aes(x = precip_meanlog )) +
  geom_histogram(bins = 20, fill = "darkorange", color = "white") +
  labs(title =
         "Distribution of Average Precipitation in Growing Season from 2013-2023",
       x = "Precipitation (in)", y = "Count") +
  theme_bw()

seasonlogprecip_dist

# does not change much with log transformation, will continue without


## relationship between variables
# Yield ~ Temperature
season_yield_temp <- ggplot(season_clean, aes(x = tavg_mean, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue") +
  labs(title = "Yield vs. Temperature",
       x = "Temperature (°F)", y = "Yield") +
  theme_bw()

season_yield_temp 


# Yield ~ Precipitation
sesaon_yield_precip <- ggplot(season_clean, aes(x = pcp_mean, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Yield vs. Precipitation",
       x = "Precipitation (in)", y = "Yield") +
  theme_bw()

sesaon_yield_precip

(seasontemp_dist + seasonprecip_dist) / (season_yield_temp + sesaon_yield_precip)
###############################################################################
# Model 5: Simple linear regression for growing season
# yield ~ precip
season_precip <- lm(yield ~ pcp_mean, data = season_clean)
summary(season_precip)

# yield ~ temp
season_temp <- lm(yield ~ tavg_mean, data = season_clean)
summary(season_temp)
###############################################################################
# Model 6: Multivariate regression model for growing season
# Fitting model

model_season <- lm(yield ~ pcp_mean + tavg_mean, data =season_clean  )
summary(model_season )


# calculate AIC 
AIC (model_season)


# Residual diagnostic plots (2 plots)
plot(model_season)
qqnorm(model_season)

###############################################################################
# Model 7: Random Effects Model for seasonal data
# Make counties into factors
season_clean1 <- season_clean %>% 
  mutate(County = factor(county_name,
                         levels = c(
                           "Butte County","Colusa County","Contra Costa County",
                           "Fresno County","Glenn County","Imperial County","Kern County",
                           "Kings County","Madera County","Merced County","Monterey County", 
                           "Sacramento County","San Benito County","San Joaquin County", 
                           "Santa Clara County","Solano County","Stanislaus County",
                           "Sutter County","Tehama County","Tulare County","Yolo County",
                           "Yuba County")))

season_clean2 <- season_clean1 %>%
  select(yield, tavg_mean, pcp_mean, County) %>%
  as.data.frame()


str(season_clean1)
# Run random effects model with seasonal averages across precip and temp

sesaon_memod <- lme(yield ~ tavg_mean + pcp_mean, 
                    random = ~1|County, data = season_clean2)

summary(sesaon_memod)

# Residual diagnostic plots (2 plots)
plot(sesaon_memod)
qqnorm(sesaon_memod)

AIC (sesaon_memod)

# Compare models with AIC
AIC(simple_temp2, simple_precip_log, linearmodel, re_model, sesaon_memod)

 
#-------------------------------no need to re-run, already saved--------------
## Precip & temp datasets
#[cleaned & wrangled datasets already save in folder, no need to re-run, just as a reference]
# precip
county_codes <- sprintf("%03d", seq(1, 115, by = 2))
make_url <- function(code) {paste0(
  "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
  "county/time-series/CA-", code,
  "/pcp/1/9/2013-2023/data.csv")}

read_noaa_csv <- function(code) {
  url <- make_url(code)
  tryCatch({
    lines <- read_lines(url)
    #Extract county name from first comment line
    header_line <- lines[str_detect(lines, "^#")][1]
    header_line <- str_remove(header_line, "^#\\s*")
    county_name <- str_extract(header_line, "^[^,]+") 
    lines_clean <- lines[!grepl("^#", lines)]
    txt <- paste(lines_clean, collapse = "\n")
    
    df <- read_csv(I(txt), show_col_types = FALSE) %>%
      clean_names()
    
    df %>%
      mutate(
        state = "CA",
        county_code = code,
        county_name = county_name,
        state_county = paste0("CA-", code),
        .before = 1
      )
    
  }, error = function(e) {
    message("Failed: ", code)
    NULL
  })
}

# run for all counties
all_df_pcp <- map_dfr(county_codes, read_noaa_csv)

# split date to year and month
all_pcp <- all_df_pcp %>%
  mutate(
    year = as.integer(substr(date, 1, 4)),
    month = as.integer(substr(date, 5, 6)))

# save as csv
write_csv(all_pcp, "ca_county_pcp_2013_2023_clean.csv")
# data scraping for temp

make_url <- function(code) {paste0(
  "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
  "county/time-series/CA-", code,
  "/tavg/1/9/2013-2023/data.csv")}

read_noaa_csv <- function(code) {
  url <- make_url(code)
  tryCatch({
    lines <- read_lines(url)
    #Extract county name from first comment line
    header_line <- lines[str_detect(lines, "^#")][1]
    header_line <- str_remove(header_line, "^#\\s*")
    county_name <- str_extract(header_line, "^[^,]+") 
    lines_clean <- lines[!grepl("^#", lines)]
    txt <- paste(lines_clean, collapse = "\n")
    
    df <- read_csv(I(txt), show_col_types = FALSE) %>%
      clean_names()
    
    df %>%
      mutate(
        state = "CA",
        county_code = code,
        county_name = county_name,
        state_county = paste0("CA-", code),
        .before = 1
      )
    
  }, error = function(e) {
    message("Failed: ", code)
    NULL
  })
}

# run for all counties
all_df <- map_dfr(county_codes, read_noaa_csv)

# split date to year and month
all_tavg <- all_df %>%
  mutate(
    year = as.integer(substr(date, 1, 4)),
    month = as.integer(substr(date, 5, 6)))

# save as csv
write_csv(all_df, "ca_county_tavg_2013_2023_clean.csv")

# XM read in data without running code for data scraping
#all_pcp <- read_csv(here("tavg_pcp_cleaned/ca_county_pcp_2013_2023_clean.csv"))
#all_df <- read_csv(here("tavg_pcp_cleaned/ca_county_tavg_2013_2023_clean.csv"))

### DATA WRANGLING
#filter for 2013-2023 years
tomato_df_2013to2023 <- tomato_df %>% 
  filter(between(Year,2013,2023)) %>% 
  filter(County != "OTHER (COMBINED) COUNTIES") %>% 
  filter(County != "OTHER COUNTIES") 
write_csv(tomato_df_2013to2023, "ca_county_yield_2013_2023_clean.csv")
# create a list for all counties included in tomato yield data
unique(tomato_df_2013to2023$County)
valid_counties <- unique(
  tomato_df_2013to2023$County[
    !tomato_df_2013to2023$County %in% c(
      "OTHER COUNTIES",
      "OTHER (COMBINED) COUNTIES")])
# join precip and temp by year and county, then filter for valid ones
# clean temperature data
variable_df <- all_tavg %>%
  rename(temp = value) %>%
  inner_join(
    all_pcp %>% rename(precip = value),
    by = c("county_code", "year", "month"))
variable_df <- variable_df %>%
  select(
    county = county_name.x,
    year,
    month,
    temp,
    precip) 
valid_counties_clean <- str_to_title(valid_counties)
variable_df <- variable_df %>%
  mutate(county_clean = str_remove(county, " County"))
final_tavg_pcp <- variable_df %>%
  filter(county_clean %in% valid_counties_clean)
# combine with yield data
tomato_clean <- tomato_df_2013to2023 %>%
  filter(!grepl("^OTHER", County)) %>%
  mutate(
    county_clean = str_to_title(County),
    year = as.integer(Year),
    tomato_value = as.numeric(gsub(",", "", Value)))
final_merged <- final_tavg_pcp %>%
  inner_join(
    tomato_clean,
    by = c("county_clean", "year"))

final_clean <- final_merged %>%
  select(
    County,
    year,
    month,
    temp,
    precip,
    yield = tomato_value,
    Data.Item)
write_csv(final_clean, "final_clean.csv")

### FIGURES
#note this is for all available counties, we may filter out to target
#certain counties
fig1 <- ggplot(tomato_df_2013to2023,
               aes(x = Year,
                   y = Value,
                   color = County))+
  geom_line()+
  labs(title = "Tomato Yields in CA per county from 2013-2023",
       caption = "Tomato Yields in CA per county from 2013-2023")
fig1

# create 2nd graph for regression
# helper function to format regression equation
get_eq_label <- function(model, vars) {
  co <- coef(model)
  r2 <- summary(model)$r.squared
  
  if (length(vars) == 1) {
    slope <- round(co[2], 3)
    intercept <- round(co[1], 3)
    
    eq <- paste0(
      "y = ", intercept,
      ifelse(slope >= 0, " + ", " - "),
      abs(slope), "(", vars, ")",
      "   |   R² = ", round(r2, 3)
    )
  } else {
    b1 <- round(co[2], 3)
    b2 <- round(co[3], 3)
    intercept <- round(co[1], 3)
    
    eq <- paste0(
      "y = ", intercept,
      ifelse(b1 >= 0, " + ", " - "), abs(b1), "(temp)",
      ifelse(b2 >= 0, " + ", " - "), abs(b2), "(precip)",
      "|R² = ", round(r2, 3)
    )
  }
  
  eq
}

# create 2nd graph for regression
# yield ~ precip + temp
model_both <- lm(yield ~ temp + precip, data = final_clean)
final_clean$predicted <- predict(model_both)
eq_both <- get_eq_label(model_both, c("temp", "precip"))

p1 <- ggplot(final_clean, aes(x = predicted, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Combined Model: Predicted vs Actual Yield",
    subtitle = eq_both,
    x = "Predicted Yield (Temp + Precip)",
    y = "Actual Yield"
  ) +
  theme_minimal()

# yield ~ precip
model_precip <- lm(yield ~ precip, data = final_clean)
eq_precip <- get_eq_label(model_precip, "precip")

p2 <- ggplot(final_clean, aes(x = precip, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(
    title = "Yield vs Precipitation",
    subtitle = eq_precip,
    x = "Precipitation",
    y = "Yield"
  ) +
  theme_minimal()

# yield ~ temp
model_temp <- lm(yield ~ temp, data = final_clean)
eq_temp <- get_eq_label(model_temp, "temp")

p3 <- ggplot(final_clean, aes(x = temp, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Yield vs Temperature",
    subtitle = eq_temp,
    x = "Temperature",
    y = "Yield"
  ) +
  theme_minimal()

final_plot <- (p2 | p3) / p1
final_plot
ggsave(
  filename = "yield_temp_precip_panel.png",
  plot = final_plot,
  width = 10,
  height = 8,
  dpi = 300)
