# MAP of Avoidable Heart Disease Death vs. Poverty

library(sf)
library(dplyr)
library(ggthemes)
library(viridis)
library(ggspatial)
library(USAboundaries)
library(albersusa) #devtools::install_github('hrbrmstr/albersusa')

# Get Data ----------------------------------------------------------------

hd_2007 <- st_read("data/Heart_Disease_Death_Rate_per_100000_45_to_64_All_Races_Ethnicities_Both_Genders_2005to2007.shp") %>% 
  st_transform(3310)

hd_2016 <- st_read("data/Heart_Disease_Death_Rate_per_100000_45_to_64_All_Races_Ethnicities_Both_Genders_2014to2016.shp") %>% 
  st_transform(3310)

comp_map <- albersusa::counties_sf() %>% st_transform(3310)
st_crs(comp_map)


# Join Data ---------------------------------------------------------------

hd_out <- left_join(comp_map, as.data.frame(hd_2016), by=c("fips"="cnty_fips"))


# Make Plot ---------------------------------------------------------------

# Total Cardiovascular Disease Death Rate per 100,000, 45-64, All Races/Ethnicities, Both Genders, 2014-2016

ggplot() +
  geom_sf(
    data = hd_out, aes(fill = happiness_score),
    size = 0.125, colour = "#2b2b2b77"
  ) +
  scale_fill_viridis(
    name = "Heart Disease \n Death Rate (per 100k)", option = "A") +
  guides(fill = guide_colorbar(title.position = "top")) +
  labs(
    title = "Total Cardiovascular Disease Death Rate",
    subtitle = "per 100,000 people (all genders, ages 45-64);",
    caption = "Data source: <https://nccd.cdc.gov/DHDSPAtlas//>\n • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  theme(axis.text = element_blank()) +
  theme(legend.title = element_text(hjust = 0.5)) +
  theme(legend.key.width = unit(2, "lines")) +
  theme(legend.position = "bottom")

