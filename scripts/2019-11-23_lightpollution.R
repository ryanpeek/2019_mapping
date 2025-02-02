# mapping light, adapted from hrbrmstr's code (y2019-30daymapchallenge)

library(raster)
library(sf)
library(tigris)
library(hrbrthemes)
library(rnaturalearth)
library(tidyverse)

ne_states("United States of America", returnclass = "sf") %>%
  filter(
    name %in% c(
      "California", "Oregon", "Nevada")) -> wcoast

urban <- urban_areas(cb = TRUE, class = "sf") %>% st_transform(st_crs(wcoast))

border <- st_union(wcoast)

wcoast_urban <- st_intersection(urban, border)

if (!all(file.exists(here::here("data", c("BlackMarble_2016_3km_geo.tif", "bm-spdf.rds"))))) {
  
  download.file(
    url = "https://eoimages.gsfc.nasa.gov/images/imagerecords/144000/144898/BlackMarble_2016_3km_geo.tif",
    destfile = here::here("data/BlackMarble_2016_3km_geo.tif")
  )
  
  if (!file.exists(here::here("bm-spdf.rds"))) {
    bm <- raster(here::here("data/BlackMarble_2016_3km_geo.tif"))
    bm <- mask(bm, as(border, "Spatial"))
    bm <- projectRaster(bm, crs = crs(albersusa::us_laea_proj))
    bm <- mask(bm, as(st_transform(border, crs(albersusa::us_laea_proj)), "Spatial"))
    
    bm_spdf <- as.data.frame(as(bm, "SpatialPixelsDataFrame"))
    colnames(bm_spdf) <- c("value", "x", "y")
    
    saveRDS(bm_spdf, here::here("data/bm-spdf.rds"))
  }
  
}


bm_spdf <- readRDS(here::here("data/bm-spdf.rds"))

ggplot() +
  geom_sf(data = wcoast, fill = "black", color = "#2b2b2b", size = 0.125) +
  geom_tile(data = bm_spdf, aes(x, y, fill = value)) +
  geom_sf(data = wcoast_urban, fill = "#54278f55", color = "#54278f", size = 0.15) +
  geom_sf(data = wcoast, fill = NA, color = "#b2b2b2", size = 0.125) +
  geom_sf(data = border, fill = NA, color = "white", size = 1/4) +
  scale_fill_distiller(name = "Brightness", palette = "Greys") +
  coord_sf(crs = albersusa::us_laea_proj, datum = NA) +
  guides(fill = guide_colourbar(title.position = "top")) +
  labs(
    x = NULL, y = NULL,
    title = "How West Coast Urban Areas\nContribute To Light Pollution",
    subtitle = "Urban areas in purple with 33% alpha overlaid on top of\nNASA Earth Observatory 'Black Marble' Light Raster",
    caption = "Data: NASA <earthobservatory.nasa.gov/features/NightLights/page3.php>; {tigris}\nhttps://https://github.com/ryanpeek/2019_mapping • #30DayMapChallenge"
  ) +
  theme_ft_rc(grid="", base_size = 8) +
  #theme(legend.position = c(0.3, 0.25)) +
  theme(legend.direction = "vertical") +
  theme(legend.key.width = unit(1, "lines"), 
        legend.key.height = unit(1, "lines")) +
  theme(panel.background = element_rect(color = "#252a32", fill = "#252a32"))

ggsave(filename = "figs/urban_areas_v_lightpollution_ca_nv_or.pdf", width = 7, height = 11, units = "in", dpi=300, device = cairo_pdf)

ggsave(filename = "figs/urban_areas_v_lightpollution_ca_nv_or.png", width = 7, height = 11, units = "in", dpi=300)
