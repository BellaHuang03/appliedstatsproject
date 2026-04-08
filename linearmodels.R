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

### read datasets
pcp_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_pcp_2013_2023_clean.csv"))
tavg_data <- read_csv(here("tavg_pcp_cleaned", "ca_county_tavg_2013_2023_clean.csv"))
final_clean <- read_csv("final_clean.csv")


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

## fit linear model
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

## fit linear model with precip log transformed
model_both_precip_log <- lm(yield ~ temp + precip_log, data = final_clean)
summary(model_both_precip_log)

# Residual diagnostic plots (4 plots)
par(mfrow = c(2, 2))
plot(model_both_precip_log)
par(mfrow = c(1, 1))

# Predicted vs. Actual
final_clean$predicted_precip_log <- predict(model_both_precip_log)

p_pred_actual_prec_log <- ggplot(final_clean, aes(x = predicted_precip_log, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Predicted vs. Actual Yield",
       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
  theme_bw()

p_pred_actual_prec_log


## fit linear model with temp squared
model_both_temp2 <- lm(yield ~ temp_squared + precip_log, data = final_clean)
summary(model_both_temp2)

# Residual diagnostic plots (4 plots)
par(mfrow = c(2, 2))
plot(model_both_temp2)
par(mfrow = c(1, 1))

# Predicted vs. Actual
final_clean$predicted_temp2 <- predict(model_both_temp2)

p_pred_actual_temp2 <- ggplot(final_clean, aes(x = predicted_temp2, y = yield)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Predicted vs. Actual Yield",
       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
  theme_bw()

p_pred_actual_temp2

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
  labs(title = "Distribution of Average Precipitation in Growgin Season from 2013-2023",
       x = "Precipitation (in)", y = "Count") +
  theme_bw()
p_precip_dist



# Fitting model

model_season <- lm(yield ~ pcp_mean + tavg_mean, data =season_clean  )
summary(model_season )

# calculate AIC 
AIC (model_season)


