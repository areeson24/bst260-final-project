# load objects
source("code/funcs.R")

# load libraries
library(tidyverse)
library(lubridate)
library(httr2)
library(janitor)
library(jsonlite)
library(purrr)

# get population data for 2020-2023
pop <- read_excel("raw-data/NST-EST2023-POP.xlsx")
colnames(pop) <- c("state", "base_pop", "2020", "2021", "2022", "2023")
pop <- pop |>
  filter(str_starts(state, "\\.")) |>
  mutate(state = str_remove(state, "^\\.")) |>
  select(-base_pop)

# estimate population data for 2024 and merge
pop_proj <- read.csv("raw-data/np2023_d1_mid.csv")
pop_proj_24 <- pop_proj |>
  filter(SEX == 0, ORIGIN == 0, RACE == 0) |>
  select(-SEX, -ORIGIN, -RACE) |>
  filter(YEAR == 2024) |>
  pull(TOTAL_POP)
pop <- pop |>
  mutate(`2020` = as.numeric(`2020`)) |>
  mutate(`2024` = `2023` / sum(`2023`) * pop_proj_24) |>
  pivot_longer(-state, names_to = "year", values_to = "population") |>
  rename(state_name = state) |>
  mutate(across(-state_name, as.numeric)) |>
  mutate(state = state.abb[match(state_name, state.name)]) |>
  mutate(state = case_when(state_name == "Puerto Rico" ~ "PR",
                           state_name == "District of Columbia" ~ "DC",
                           .default = state))

# get regions data
url <- "https://github.com/datasciencelabs/2024/raw/refs/heads/main/data/regions.json"
regions <- fromJSON(url, simplifyDataFrame = FALSE) |> 
  map_df(function(x) {data.frame(region = x$region,
                                 region_name = x$region_name,
                                 state_name = x$states)}) |>
  mutate(region_name = if_else(
  region_name == "New York and New Jersey, Puerto Rico, Virgin Islands", 
                               "NY and NJ, PR, VI",
                               region_name))

# combine data sets
pop <- pop |> left_join(regions, by = "state_name")

# obtain COVID-19 data
cases_raw <-  get_cdc_data("https://data.cdc.gov/resource/pwn4-m3yp.json")
hosp_raw <- get_cdc_data("https://data.cdc.gov/resource/39z2-9zu6.json")
deaths_raw <- get_cdc_data("https://data.cdc.gov/resource/r8kw-7aab.json")
vax_raw <- get_cdc_data("https://data.cdc.gov/resource/rh2h-3yt2.json")

# wrangle cases
cases <- cases_raw |>
  mutate(cases = parse_number(new_cases), date = as_date(ymd_hms(end_date))) |>
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |>
  filter(state %in% pop$state) |>
  select(state, mmwr_year, mmwr_week, cases) |>
  arrange(state, mmwr_year, mmwr_week)

# wrangle hospitalizations
hosp <- hosp_raw |>
  filter(jurisdiction %in% pop$state) |>
  rename(hosp = new_covid_19_hospital, state = jurisdiction) |>
  mutate(hosp = parse_number(hosp), date = as_date(ymd_hms(collection_date))) |>
  mutate(mmwr_week = epiweek(date), mmwr_year = epiyear(date)) |>
  select(state, mmwr_year, mmwr_week, hosp) |>
  group_by(state, mmwr_year, mmwr_week) |>
  summarize(hosp = sum(hosp), n = n(), .groups = "drop") |>
  filter(n == 7) |>
  select(-n) |>
  arrange(state, mmwr_year, mmwr_week)

# wrangle deaths
deaths <- deaths_raw |>
  filter(group == "By Week") |>
  rename(state_name = state) |>
  filter(state_name %in% pop$state_name) |>
  rename(deaths = covid_19_deaths) |>
  mutate(deaths = parse_number(deaths),
         date = as_date(ymd_hms(end_date))) |>
  mutate(mmwr_week = epiweek(date),
         mmwr_year = epiyear(date)) |>
  select(state_name, mmwr_year, mmwr_week, deaths) |>
  arrange(state_name, mmwr_year, mmwr_week)

# wrangle vaccinations
vax <- vax_raw |>
  mutate(booster = booster_cumulative,
         series_complete = series_complete_cumulative,
         state = location) |>
  filter(state %in% pop$state) |>
  mutate(booster = parse_number(booster),
         series_complete = parse_number(series_complete),
         date = as_date(ymd_hms(date))) |>
  mutate(mmwr_week = epiweek(date),
         mmwr_year = epiyear(date)) |>
  select(state, mmwr_year, mmwr_week, series_complete, booster) |>
  group_by(state, mmwr_year, mmwr_week) |>
  summarize(series_complete = max(series_complete),
            booster = max(booster),
            .groups = "drop") |>
  arrange(state, mmwr_year, mmwr_week)

# create date frame with all dates
all_dates <- data.frame(date = seq(make_date(2020, 1, 1),
                                   make_date(2024, 12, 31), 
                                   by = "week")) |>
  mutate(date = ceiling_date(date, unit = "week", week_start = 7) - days(1)) |>
  mutate(mmwr_year = epiyear(date), mmwr_week = epiweek(date)) 

# merge with population data
dates_and_pop <- cross_join(all_dates, 
                            data.frame(state = unique(pop$state))) |> 
  left_join(pop, by = c("state", "mmwr_year" = "year"))

# join all tables
dat <- dates_and_pop |>
  arrange(state, mmwr_year, mmwr_week) |>
  left_join(cases, by = c("state", "mmwr_year", "mmwr_week")) |>
  left_join(hosp, by = c("state", "mmwr_year", "mmwr_week")) |>
  left_join(deaths, by = c("state_name", "mmwr_year", "mmwr_week")) |>
  left_join(vax, by = c("state", "mmwr_year", "mmwr_week"))

# save as RDS
dat |> saveRDS("data/dat.rds")
