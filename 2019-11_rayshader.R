# test of rayshader
library(rayshader)
library(sf)
library(tidyverse)

#  Create Data ------------------------------------------------------------

a = data.frame(x = rnorm(20000, 10, 1.9), y = rnorm(20000, 10, 1.2))
b = data.frame(x = rnorm(20000, 14.5, 1.9), y = rnorm(20000, 14.5, 1.9))
c = data.frame(x = rnorm(20000, 9.5, 1.9), y = rnorm(20000, 15.5, 1.9))
data = rbind(a, b, c)

# Basic Lines Hex ---------------------------------------------------------

#Lines
(pp = ggplot(data, aes(x = x, y = y)) +
  geom_hex(bins = 20, size = 0.5, color = "black") +
  scale_fill_viridis_c(option = "C"))

par(mfrow = c(1, 2))
plot_gg(pp, width = 5, height = 4, scale = 300, raytrace = FALSE, preview = TRUE)
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, windowsize = c(1000, 800))
render_camera(fov = 70, zoom = 0.5, theta = 130, phi = 35)
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

# Render with Focus Spot --------------------------------------------------

par(mfrow = c(1, 1))
plot_gg(pp, width = 5, height = 4, scale = 300, multicore = TRUE, 
        windowsize = c(1200, 960), fov = 70, zoom = 0.4, 
        theta = 330, phi = 40)
Sys.sleep(0.2)

# to render to local Plot window
render_depth(focus = 0.68, focallength = 200)

# to render directly to outfile
render_depth(filename = "figs/rayshader_hex_blurry_focus_v2.pdf", 
             focus = 0.68, focallength = 200)


# Now with chloropleths ---------------------------------------------------
library(rayshader)
library(sf)
library(ggthemes)
library(viridis)

# SAC from here: 
# https://data.cityofsacramento.org/datasets/b3047674f3f04a759c484fe5208faf6c_0/data

# la trees
# trees <- st_read("data/LACO_PARK_TREES_PHASES1_AND_2.shp") %>% 
#   st_transform(3310) %>% 
#   dplyr::filter(!AREA=="Apollo Community Regional Park")
# 
# plot(trees$geometry)
# 
# (gg_trees <- ggplot(data=trees, aes(x=EASTING, y=NORTHING, color=CANOPY)) + 
#     geom_hex(bins = 50, size = 1)+
#     scale_fill_viridis("Canopy %") +
#     theme_classic()
    #ggdark::dark_theme_bw(base_family = "Roboto Condensed")
# )

# plot_gg(gg_trees, width = 5, height = 4, scale = 300, multicore = TRUE, 
#         windowsize = c(1200, 960), fov = 70, zoom = 0.4, 
#         theta = 330, phi = 40)
# 

# shps
sac_parks <- st_read("data/sac/Parks.shp")
sac_park_restrooms <- st_read("data/sac/Parks_Public_Restrooms.shp")
sac_census <- st_read("data/sac/Census_2010_Block_Population.shp")

(gg_sac <- ggplot() + 
    geom_sf(data=sac_census, aes(fill=log(POP_SQ_MIL)), lwd=0.01, color=NA)+
    #geom_sf(data=sac_census, color="gray80", alpha=0.2, lwd=0.1) +
    scale_fill_viridis_c("Census Data") +
    #guides(fill = guide_colorbar(title.position = "top"))+
    #theme_bw() +
    #ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          legend.title = element_text(hjust = 0.5),
          legend.key.width = unit(1.5, "lines"),
          legend.key.height = unit(0.8, "lines"))
          #legend.position = "bottom")
)

plot_gg(gg_sac, width = 5, height = 4, scale = 100, multicore = TRUE, 
        windowsize = c(1200, 960), fov = 70, zoom = 0.4, 
        theta = 330, phi = 40)


# reset defaults
#invert_geom_defaults()

(gg_parks <- ggplot() +
    #geom_sf(data=sac_census, color="gray80", alpha=0.2, lwd=0.1) +
    geom_sf(data=sac_parks, aes(fill=TYPE), lwd=0.1, color="gray80")+
    scale_fill_viridis("Park Types", discrete = T) +
    theme_classic(base_family = "Roboto Condensed") +
    #ggdark::dark_theme_bw(base_family = "Roboto Condensed") +
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())
#          plot.background = element_rect(fill = "white", color=NA))
)

# 3d
plot_gg(gg_parks, width = 6, height = 4, height_aes = "Shape__Are",
        scale = 150, multicore = TRUE, 
        windowsize = c(1200, 960), fov = 70, zoom = 0.4, 
        theta = 330, phi = 40)



# Tracks with Rayshader: --------------------------------------------------

#https://www.elizabetheaster.com/blog/2019/07/19/GPS_Routes_Plotted_on_Realistic_3D_Map


