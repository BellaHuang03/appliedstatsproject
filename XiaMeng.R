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
library(nlme)

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
  labs(title = "Distribution of Precipitation",
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
  labs(title = "Distribution of Yield",
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

# Yield ~ Precipitation
p_yield_precip <- ggplot(final_clean, aes(x = precip, y = yield)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "darkorange") +
  labs(title = "Yield vs. Precipitation",
       x = "Precipitation (in)", y = "Yield") +
  theme_bw()

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

###############################################################################
## fit linear model with precip log transformed
#model_both_precip_log <- lm(yield ~ temp + precip_log, data = final_clean)
#summary(model_both_precip_log)

# Residual diagnostic plots (4 plots)
#par(mfrow = c(2, 2))
#plot(model_both_precip_log)
#par(mfrow = c(1, 1))

# Predicted vs. Actual
#final_clean$predicted_precip_log <- predict(model_both_precip_log)

#p_pred_actual_prec_log <- ggplot(final_clean, aes(x = predicted_precip_log, y = yield)) +
#  geom_point(alpha = 0.6) +
#  geom_smooth(method = "lm", se = TRUE, color = "red") +
#  labs(title = "Predicted vs. Actual Yield",
#       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
#  theme_bw()

#p_pred_actual_prec_log

###############################################################################
## fit linear model with temp squared AND precip logged
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
## fit linear model with temp squared and normal precip
#model_both_temp2_prec <- lm(yield ~ temp_squared + precip, data = final_clean)
#summary(model_both_temp2_prec)

# Residual diagnostic plots (4 plots)
#par(mfrow = c(2, 2))
#plot(model_both_temp2_prec)
#par(mfrow = c(1, 1))

# Predicted vs. Actual
#final_clean$predicted_temp2_prec <- predict(model_both_temp2_prec)

#p_pred_actual_temp2_prec <- ggplot(final_clean, aes(x = predicted_temp2_prec, y = yield)) +
#  geom_point(alpha = 0.6) +
#  geom_smooth(method = "lm", se = TRUE, color = "red") +
#  labs(title = "Predicted vs. Actual Yield",
#       x = "Predicted Yield (Temp + Precip)", y = "Actual Yield") +
#  theme_bw()

#p_pred_actual_temp2_prec

###############################################################################
# Random Effects Model
# Make counties into factors
final_clean <- final_clean %>% 
  mutate(County = factor(County, levels = c("BUTTE", "COLUSA", "CONTRA COSTA", "FRESNO", "GLENN", 
                                            "IMPERIAL", "KERN", "KINGS", "MADERA", "MERCED", "MONTEREY", 
                                            "SACRAMENTO", "SAN BENITO", "SAN JOAQUIN", "SANTA CLARA", 
                                            "SOLANO", "STANISLAUS", "SUTTER", "TEHAMA", "TULARE", 
                                            "YOLO", "YUBA")))

# Run random effects model with temp squared and precip logged
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






