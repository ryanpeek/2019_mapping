# 2019_ca exports
# WHAT!?! https://www.census.gov/foreign-trade/statistics/state/data/ca.html

library(rvest)
library(sf)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(hrbrthemes)
library(tidyverse)


# read the web page
pg <- read_html("https://www.census.gov/foreign-trade/statistics/state/data/ca.html")

# table 1 
html_nodes(pg, "table") %>%  # parse the page
  .[[1]] %>%
  html_table() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  slice(-c(1:2)) #%>% 
  select(country, x2018_value) %>%
  mutate(x2018_value = parse_number(x2018_value)) %>%
  filter(!grepl("Total", country)) %>%
  mutate(country = case_when( # clean up the countries
    country == "Macau" ~ "Macao",
    country == "Korea, South" ~ "Korea",
    TRUE ~ country
  )) %>%
  arrange(desc(x2018_value)) %>%
  slice(1:20) %>%
  left_join(world, by = c("country" = "name")) %>% # get the geometries for the target countries so we can get their centers
  mutate(to = suppressWarnings(st_centroid(geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>% # need to transform (see note below)
  select(country, x2018_value, to) %>%
  mutate(maine = maine_center) -> xdf # temporary variable


# table 2 
html_nodes(pg, "table") %>%  # parse the page
  .[[2]] %>%
  html_table() %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  select(country, x2018_value) %>%
  mutate(x2018_value = parse_number(x2018_value)) %>%
  filter(!grepl("Total", country)) %>%
  mutate(country = case_when( # clean up the countries
    country == "Macau" ~ "Macao",
    country == "Korea, South" ~ "Korea",
    TRUE ~ country
  )) %>%
  arrange(desc(x2018_value)) %>%
  slice(1:20) %>%
  left_join(world, by = c("country" = "name")) %>% # get the geometries for the target countries so we can get their centers
  mutate(to = suppressWarnings(st_centroid(geometry)) %>% st_transform(crs = "+proj=eqearth +wktext")) %>% # need to transform (see note below)
  select(country, x2018_value, to) %>%
  mutate(maine = maine_center) -> xdf # temporary variable