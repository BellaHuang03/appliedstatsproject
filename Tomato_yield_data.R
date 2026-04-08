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

### IMPORT DATASETS
## Tomato yields
#read all tomato yield csv file
tomato_df <- read.csv("CA_tomato_yield_by_county.csv", header = TRUE)

#filter for 2013-2023 years
tomato_df_2013to2023 <- tomato_df %>% 
  filter(between(Year,2013,2023)) %>% 
  filter(County != "OTHER (COMBINED) COUNTIES") %>% 
  filter(County != "OTHER COUNTIES") 
write_csv(tomato_df_2013to2023, "ca_county_yield_2013_2023_clean.csv")

## Precip & temp datasets [cleaned datasets already save in folder, no need to re-run]
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


