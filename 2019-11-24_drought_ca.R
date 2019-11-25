# getting drought info for last 10 years

# drought index
# Evaporative Demand Drought Index (EDDI)
# https://www.earthdatascience.org/eddi/articles/eddi-roi-tutorial.html
# examines how anomalous the atmospheric evaporative demand (E0; also known as "the thirst of the atmosphere") is for a given location and across a time period of interest

library(eddi)
library(tidyverse)
library(hrbrthemes)
library(sf)
library(rnaturalearth)
library(raster)
library(tidyverse)


# Get States --------------------------------------------------------------

ne_states("United States of America", returnclass = "sf") %>%
  filter(
    name %in% c(
      "Washington","Oregon","Nevada","Arizona","California"
    )) -> wus

border <- st_union(wus)

# weekly
#eddi_data <- get_eddi(date = "2019-11-15", timescale = "1 week")


# 2019: Get data ----------------------------------------------------------------

# get monthly
d19nov <- get_eddi(date = "2019-11-15", timescale = "1 month")

eddi <- mask(d19nov, as(border, "Spatial"))
eddi <- projectRaster(eddi, crs = crs(albersusa::us_laea_proj))
eddi <- mask(eddi, as(st_transform(border, crs(albersusa::us_laea_proj)), "Spatial"))
eddi_spdf <- as.data.frame(as(eddi, "SpatialPixelsDataFrame"))
colnames(eddi_spdf) <- c("value", "x", "y")
saveRDS(eddi_spdf, here::here("data/eddi_20191115_monthly_spdf.rds"))

d19nov <- readRDS(here::here("data/eddi_20191115_monthly_spdf.rds"))

# plot
(ed19 <- ggplot() +
    geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
    geom_tile(data = d19nov, aes(x, y, fill = value)) +
    geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
    geom_sf(data = border, fill = NA, color = "white", size = .25) +
    scale_fill_viridis_c(name = "EDDI", option = "A") +
    coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
    guides(fill = guide_colourbar(title.position = "top")) +
    labs(
      x = NULL, y = NULL,
      title = "2019-Nov-15: EDDI (monthly)",#\n(Evaporative Demand Drought Index)",
      subtitle = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)",
      caption = "Data: <https://www.earthdatascience.org/eddi>\nCode: <https://github.com/ryanpeek/2019_mapping/>") +
    expand_limits(x = 0, y=0)+
    theme_ft_rc(subtitle_size = 11, grid="") +
    #theme_ft_rc(plot_title_size = 10, subtitle_size = 7, grid="") +
    theme(legend.position = c(0.75, 0.7)) +
    theme(legend.direction = "vertical") +
    theme(legend.key.width = unit(1.2, "lines"),
          legend.key.height = unit(1, "lines")) +
    theme(panel.background = element_rect(color = "#252a32", fill = "#252a32")))

ggsave(filename = "figs/eddi_westcoast_2019_nov.pdf", width = 7, height = 11, units = "in", dpi=300, device = cairo_pdf)

# 2014: Get Data ----------------------------------------------------------

# get monthly
d14nov <- get_eddi(date = "2014-11-15", timescale = "1 month")

eddi <- mask(d14nov, as(border, "Spatial"))
eddi <- projectRaster(eddi, crs = crs(albersusa::us_laea_proj))
eddi <- mask(eddi, as(st_transform(border, crs(albersusa::us_laea_proj)), "Spatial"))
eddi_spdf <- as.data.frame(as(eddi, "SpatialPixelsDataFrame"))
colnames(eddi_spdf) <- c("value", "x", "y")
saveRDS(eddi_spdf, here::here("data/eddi_20141115_monthly_spdf.rds"))

d14nov <- readRDS(here::here("data/eddi_20141115_monthly_spdf.rds"))

# plot
(ed14 <- ggplot() +
    geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
    geom_tile(data = d14nov, aes(x, y, fill = value)) +
    geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
    geom_sf(data = border, fill = NA, color = "white", size = .25) +
    scale_fill_viridis_c(name = "EDDI", option = "A") +
    coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
    guides(fill = guide_colourbar(title.position = "top")) +
    labs(
      x = NULL, y = NULL,
      title = "2014-Nov-15: EDDI (monthly)",
      subtitle = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)",
      # title = "2014-Nov-15: EDDI\n(Evaporative Demand Drought Index)",
      # subtitle = "30-day avg. of how anomalous the\natmospheric evaporative demand is\n(the thirst of the atmosphere)",
      caption = "Data: <https://www.earthdatascience.org/eddi>\nCode: <https://github.com/ryanpeek/2019_mapping/>") +
    expand_limits(x = 0, y=0)+
    theme_ft_rc(subtitle_size = 11, grid="") +
    #theme_ft_rc(plot_title_size = 10, subtitle_size = 7, grid="") +
    theme(legend.position = c(0.75, 0.7)) +
    theme(legend.direction = "vertical") +
    theme(legend.key.width = unit(1.2, "lines"),
          legend.key.height = unit(1, "lines")) +
    theme(panel.background = element_rect(color = "#252a32", fill = "#252a32")))

ggsave(filename = "figs/eddi_westcoast_2014_nov.pdf", width = 7, height = 11, units = "in", dpi=300, device = cairo_pdf)


# 2015: Get Data ----------------------------------------------------------

# get monthly
d15nov <- get_eddi(date = "2015-11-15", timescale = "1 month")

eddi <- mask(d15nov, as(border, "Spatial"))
eddi <- projectRaster(eddi, crs = crs(albersusa::us_laea_proj))
eddi <- mask(eddi, as(st_transform(border, crs(albersusa::us_laea_proj)), "Spatial"))
eddi_spdf <- as.data.frame(as(eddi, "SpatialPixelsDataFrame"))
colnames(eddi_spdf) <- c("value", "x", "y")
saveRDS(eddi_spdf, here::here("data/eddi_20151115_monthly_spdf.rds"))

d15nov <- readRDS(here::here("data/eddi_20151115_monthly_spdf.rds"))

# plot
(ed15 <- ggplot() +
  geom_sf(data = wus, fill = "black", color = "#2b2b2b", size = 0.12) +
  geom_tile(data = d15nov, aes(x, y, fill = value)) +
  geom_sf(data = wus, fill = NA, color = "#b2b2b2", size = 0.125) +
  geom_sf(data = border, fill = NA, color = "white", size = .25) +
  scale_fill_viridis_c(name = "EDDI", option = "A") +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = "2015-Nov-15: EDDI (monthly)",
    subtitle = "EDDI: Evaporative Demand Drought Index of\nhow anomalous the atmospheric evaporative\ndemand is (the thirst of the atmosphere)",
    caption = "Data: <https://www.earthdatascience.org/eddi>\nCode: <https://github.com/ryanpeek/2019_mapping/>") +
    expand_limits(x = 0, y=0)+
    theme_ft_rc(subtitle_size = 11, grid="") +
    #theme_ft_rc(plot_title_size = 10, subtitle_size = 7, grid="") +
    theme(legend.position = c(0.75, 0.7)) +
    theme(legend.direction = "vertical") +
    theme(legend.key.width = unit(1.2, "lines"),
          legend.key.height = unit(1, "lines")) +
    theme(panel.background = element_rect(color = "#252a32", fill = "#252a32")))

ggsave(filename = "figs/eddi_westcoast_2015_nov.pdf", width = 7, height = 11, units = "in", dpi=300, device = cairo_pdf)

# Cowplot -----------------------------------------------------------------

library(cowplot)

(comb_plot <- plot_grid(ed15, ed19, nrow = 1))
(comb_plot <- plot_grid(ed14, ed15, ed19, nrow = 1))

cowplot::save_plot(
  plot = comb_plot, base_height = 8, base_width = 12,
  filename = "figs/eddi_drought_westcoast_2014_2015_2019_nov.pdf",
  device = cairo_pdf
)

cowplot::save_plot(
  plot = comb_plot, base_height = 8, base_width = 12, units="in", dpi=300,
  filename = "figs/eddi_drought_westcoast_2014_2015_2019_nov.png"
)

# Data --------------------------------------------------------------------


d18nov <- get_eddi(date = "2018-11-15", timescale = "1 month")
d17nov <- get_eddi(date = "2017-11-15", timescale = "1 month")
d16nov <- get_eddi(date = "2016-11-15", timescale = "1 month")
d15nov <- get_eddi(date = "2015-11-15", timescale = "1 month")
d14nov <- get_eddi(date = "2014-11-15", timescale = "1 month")


# Setup Plots -------------------------------------------------------------

# set up colors
#color_pal <- colorRampPalette(c("blue", "lightblue", "white", "pink", "red"))

color_pal <- viridis::viridis_pal(option = "A")

# weekly
#raster::plot(eddi_data, col = color_pal(255), main = "EDDI data for 2019-11-15")

# rasters
raster::plot(d14nov, col = color_pal(255), main = "EDDI 2014-11-15")
raster::plot(d15nov, col = color_pal(255), main = "EDDI 2015-11-15")
raster::plot(d16nov, col = color_pal(255), main = "EDDI 2016-11-15")
