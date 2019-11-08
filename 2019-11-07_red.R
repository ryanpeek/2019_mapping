# MAP of Avoidable Heart Disease Death vs. Poverty

library(sf)
library(dplyr)
library(ggthemes)
library(hrbrthemes)
library(viridis)
library(ggspatial)
library(USAboundaries)
library(albersusa) #devtools::install_github('hrbrmstr/albersusa')

# Get Data ----------------------------------------------------------------

# 2019-11-07: downloaded as shapefiles from https://nccd.cdc.gov/DHDSPAtlas/

# hd_2007 <- st_read("data/Heart_Disease_Death_Rate_per_100000_45_to_64_All_Races_Ethnicities_Both_Genders_2005to2007.shp") %>%
#   st_transform(3310)
# 
# hd_2016 <- st_read("data/Heart_Disease_Death_Rate_per_100000_45_to_64_All_Races_Ethnicities_Both_Genders_2014to2016.shp") %>%
#   st_transform(3310)
# 
# pov <- st_read("data/Percentage_Living_in_Poverty_All_Ages_2016.shp") %>%
#   st_transform(3310)

# from hrbrmstr/albersusa
# comp_map <- albersusa::counties_sf() %>% st_transform(3310)
# st_crs(comp_map)

# save these data back out for pushing to git, # add delete_layer if needing to overwrite completely
# st_write(hd_2007, dsn = "data/mapping_data_sf.gpkg", layer = "cardio_death_rate_per_100k_45_to_65_all_ethnicity_genders_2005_2007") 
# st_write(hd_2016, dsn = "data/mapping_data_sf.gpkg", layer = "cardio_death_rate_per_100k_45_to_65_all_ethnicity_genders_2014_2016")
# st_write(pov, dsn = "data/mapping_data_sf.gpkg", layer = "percentage_living_in_poverty_all_ages_2106")
# 
# st_write(comp_map, dsn = "data/mapping_data_sf.gpkg", layer = "us_counties_w_AK_HI")


# Load Data ---------------------------------------------------------------

# the data is in here:
geoDB <- "data/mapping_data_sf.gpkg" # the geopackage

# see what's in the geopackage
st_layers(dsn = geoDB)

# heart disease death rate 2005-2007
hd_2007 <- st_read(dsn = geoDB, layer = "cardio_death_rate_per_100k_45_to_65_all_ethnicity_genders_2005_2007") %>% 
  st_set_crs(value = 3310)
class(hd_2007) # check data type
st_crs(hd_2007) # check projection

# heart disease death rate 2014-2016
hd_2016 <- st_read(dsn=geoDB, layer = "cardio_death_rate_per_100k_45_to_65_all_ethnicity_genders_2014_2016") %>% 
  st_set_crs(value = 3310)

# % living in poverty
pov <- st_read(dsn=geoDB, "percentage_living_in_poverty_all_ages_2106") %>%
  st_set_crs(value=3310)

# from hrbrmstr/albersusa
comp_map <- albersusa::counties_sf() %>% st_transform(3310)

# Join Data ---------------------------------------------------------------

# join the hd data with the county map data
hd_out <- left_join(comp_map, as.data.frame(hd_2016), by=c("fips"="cnty_fips"))

# join poverty data
pov_out <- left_join(comp_map, as.data.frame(pov), by=c("fips"="cnty_fips"))

# Make Plot of Heart Disease ----------------------------------------

# make color palette
# by_pal <- colorRampPalette(c("#FFF5EE", "#EEA2AD", "#F08080", "#CD0000","#8B0000"))(20)

by_pal <- RColorBrewer::brewer.pal(n = 9, name = "Reds") 

# Total Cardiovascular Disease Death Rate per 100,000, 45-64, All Races/Ethnicities, Both Genders, 2014-2016

(cd_2016 <- ggplot() +
    geom_sf(
      data = hd_out, aes(fill = Value),
      size = 0.125, colour = "#2b2b2b77"
    ) +
    scale_fill_gradientn(
      colors = by_pal,
      name = "Heart Disease \n Death Rate 2014-2016") +
    guides(fill = guide_colorbar(title.position = "top")) +
    labs(
      title = "Total Cardiovascular \n Disease Death Rate",
      subtitle = "per 100,000 people \n(all genders, ages 45-64)",
      caption = "Data source: <https://nccd.cdc.gov/DHDSPAtlas/>\nhttps://github.com/ryanpeek/2019_mapping • #30DayMapChallenge"
    ) +
    ggdark::dark_theme_minimal(base_family = "Roboto Condensed")+
    theme(axis.line = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank())+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    theme(axis.text = element_blank()) +
    theme(legend.title = element_text(hjust = 0.5)) +
    theme(legend.key.width = unit(2, "lines")) +
    theme(legend.position = "bottom")
)

#ggsave("figs/cardiovasc_death_2014-16_45-64_all_gender.png", width = 9.5, height = 6, dpi = 300, units = "in")

# Make Plot of Poverty ----------------------------------------------------

#by_pal <- RColorBrewer::brewer.pal(n = 11, name = "RdYlBu") 

# now poverty
(pov_map <- ggplot() +
   geom_sf(
     data = pov_out, aes(fill = Value),
     size = 0.125, colour = "#2b2b2b77"
   ) +
    scale_fill_viridis(option = "B", name = "% in Poverty") +
   guides(fill = guide_colorbar(title.position = "top")) +
   labs(
     title = "Percent Living in Poverty",
     subtitle = "All Ages, 2016",
     caption = "Data source: <https://nccd.cdc.gov/DHDSPAtlas/>\nhttps://github.com/ryanpeek/2019_mapping • #30DayMapChallenge"
   ) +
   ggdark::dark_theme_minimal(base_family = "Roboto Condensed") +
   theme(axis.line = element_blank(),
         axis.line.x = element_blank(),
         axis.line.y = element_blank()) +
   theme(plot.title = element_text(hjust = 0.5)) +
   theme(plot.subtitle = element_text(hjust = 0.5)) +
   theme(axis.text = element_blank()) +
   theme(legend.title = element_text(hjust = 0.5)) +
   theme(legend.key.width = unit(2, "lines")) +
   theme(legend.position = "bottom")
)

#ggsave("figs/percent_living_in_poverty_2016.png", width = 9.5, height = 6, dpi = 300, units = "in")


# Combine -----------------------------------------------------------------

library(cowplot)

# combine plots
plots <- plot_grid(cd_2016, pov_map, nrow = 1)

#cowplot::save_plot(plots, filename = "figs/poverty_vs_heart_disease_2016.png", base_width = 11, base_height =  8.5, dpi = 300, units = "in")
