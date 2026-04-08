## Precip & temp datasets [cleaned datasets already save in folder, no need to re-run]
library(tidyverse)
# new data scraping for March to June
# precip
county_codes <- sprintf("%03d", seq(1, 115, by = 2))
make_url <- function(code) {paste0(
  "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
  "county/time-series/CA-", code,
  "/pcp/1/6/2013-2023/data.csv")}

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
write_csv(all_pcp, "ca_county_pcp_2013_2023_clean_6.csv")
# data scraping for temp

make_url <- function(code) {paste0(
  "https://www.ncei.noaa.gov/access/monitoring/climate-at-a-glance/",
  "county/time-series/CA-", code,
  "/tavg/1/6/2013-2023/data.csv")}

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
write_csv(all_df, "ca_county_tavg_2013_2023_clean_6.csv")

# join all pcp and tavg tgt and take average
# PCP files
pcp_files <- list.files(
  path = "~/R/appliedstat",
  pattern = "ca_county_pcp_2013_2023_clean_.*\\.csv",
  full.names = TRUE
)

all_pcp <- map_dfr(pcp_files, read_csv)

# keep only March–June (extra safety)
all_pcp <- all_pcp %>%
  filter(month %in% 3:6) %>%
  arrange(state, county_code, year, month)

tavg_files <- list.files(
  path = "~/R/appliedstat",
  pattern = "ca_county_tavg_2013_2023_clean_.*\\.csv",
  full.names = TRUE
)

all_tavg <- map_dfr(tavg_files, read_csv)

all_tavg <- all_tavg %>%
  filter(month %in% 3:6) %>%
  arrange(state, county_code, year, month)

all_pcp <- all_pcp %>%
  mutate(month_name = month.name[month])

all_tavg <- all_tavg %>%
  mutate(month_name = month.name[month])

all_pcp <- all_pcp %>%
  rename(pcp = value)

all_tavg <- all_tavg %>%
  rename(tavg = value)

# final save
all_tavg <- all_tavg %>%
  mutate(
    date = as.character(date),
    year = as.integer(substr(date, 1, 4)),
    month = as.integer(substr(date, 5, 6)),
    month_name = month.name[month]
  )

season_df <- all_pcp %>%
  left_join(
    all_tavg,
    by = c("state", "county_code", "county_name", "year", "month")
  )

season_df <- all_pcp %>%
  select(state, county_code, county_name, year, month, pcp) %>%
  left_join(
    all_tavg %>%
      select(state, county_code, year, month, tavg),
    by = c("state", "county_code", "year", "month")
  )

season_avg <- season_df %>%
  group_by(state, county_code, county_name, year) %>%
  summarise(
    pcp_mean = mean(pcp, na.rm = TRUE),
    tavg_mean = mean(tavg, na.rm = TRUE),
    n_months = n(),   # sanity check
    .groups = "drop"
  )
write_csv(season_avg, "average_pcp_tavg_season_2013_2023.csv")

# join it back with tomato yield data
# create a list for all counties included in tomato yield data
tomato_df_2013to2023 <- read.csv("ca_county_yield_2013_2023_clean.csv", header = TRUE)

unique(tomato_df_2013to2023$County)
valid_counties <- unique(
  tomato_df_2013to2023$County[
    !tomato_df_2013to2023$County %in% c(
      "OTHER COUNTIES",
      "OTHER (COMBINED) COUNTIES")])
# join precip and temp by year and county, then filter for valid ones
# clean temperature data
tomato_clean <- tomato_df_2013to2023 %>%
  filter(Geo.Level == "COUNTY") %>%
  mutate(
    state = "CA",
    year = Year,
    county_name = str_to_title(County)  # adjust if column name differs
  ) %>%
  select(state, county_name, year, yield = Value)
tomato_clean <- tomato_clean %>%
  mutate(
    county_name = paste0(county_name, " County")
  )
season_final <- season_avg %>%
  left_join(
    tomato_clean,
    by = c("state", "county_name", "year")
  )%>%
  filter(!is.na(yield))
summary(season_final$yield)
write_csv(season_final, "season_final.csv")
