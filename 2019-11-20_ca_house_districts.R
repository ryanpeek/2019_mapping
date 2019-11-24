
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(rvest)
library(tigris) #devtools::install_github('walkerke/tigris')
library(hrbrthemes) #devtools::install_github("hrbrmstr/hrbrthemes")
library(sf)

# https://raw.githubusercontent.com/CivilServiceUSA/us-states/master/data/states.json

# https://en.wikipedia.org/wiki/2018_United_States_House_of_Representatives_elections_in_California

# read webpage
pg <- read_html("https://en.wikipedia.org/wiki/2018_United_States_House_of_Representatives_elections_in_California")

house_df <- html_nodes(pg, "table") %>%  # parse the page
  .[[10]] %>%
  html_table(fill = TRUE) %>%
  as_tibble(.name_repair = "unique") %>%
  janitor::clean_names() %>%
  slice(-1, -2) %>% 
  set_names(c("district", "democractic_votes", "democratic_%", "republican_votes", "republican_%", "others_votes", "others_%", "total_votes", "total_%", "result"))

# Get CA house ("lower") boundaries ---------------------------------------

ca_house <- state_legislative_districts(state = "California", house = "lower", cb = TRUE, class="sf", year=2018)

ca_house <- ca_house %>% st_transform(4326)
# st_crs(ca_house)
plot(ca_house$geometry)


# Make the Plot -----------------------------------------------------------

# this crashes EVERY SINGLE TIME
ggplot() + geom_sf(data=ca_house, aes(fill=NAME), color = "white", size = 0.25) + 
  coord_sf(crs=albersusa::us_laea_proj, datum = NA) +
  geom_sf_text(data=ca_house, aes(label=NAME), size=2, color="white") +
  labs(x = NULL, y = NULL,
       title = "2018 CA House District Borders",
       caption = "Data source: {tigris}\nhttps://github.com/ryanpeek/2019_mapping 30DayMapChallenge"
  ) + 
  theme_ft_rc(grid="") +
  theme(legend.position = c(0.5, 0.95)) +
  theme(legend.position = "horizontal")
  