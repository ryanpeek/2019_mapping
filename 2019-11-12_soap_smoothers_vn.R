# SOAP Smoothers


# Libraries ---------------------------------------------------------------

library(brms)
library(elevatr)
library(tidyverse)
library(mapview)
library(sf)
library(viridis)
#devtools::install_github("khufkens/MODISTools")
#library(MODISTools)
library(mgcv)

# Van Norden --------------------------------------------------------------

# spatial data
load("data/vn_spatial_data.rda")
# well data
wells_rtk <- readRDS(file = "data/wells_rtk_joined_rev.rds")

# make spatial
wells_rtk_sf <- st_as_sf(wells_rtk, 
                         coords = c("Easting", "Northing"),
                         remove = F,
                         crs = 26910) %>%
  st_transform(3310) %>% 
  #distinct(wellID, .keep_all = T) %>% 
  #dplyr::select(wellID, transectID, RTK_id_base:geometry)
  dplyr::select(wellID, transectID, YYYY, MM, stage_m, DOWY:DOWY,RTK_id_base:geometry)

# transform
vn_stream_ctrlpts <- vn_stream_ctrlpts %>% st_transform(3310)
vn_mdw_boundary <- vn_mdw_boundary %>% st_transform(3310)
vn_lake_boundary <- vn_lake_boundary %>% st_transform(3310)
st_crs(vn_mdw_boundary)

# Get Raster Data ---------------------------------------------------------

library(raster)

# get raster for VN (3310)
elev_img<-get_elev_raster(vn_mdw_boundary, z = 14, clip = "locations")

# make into matrix
elev_matrix <- matrix(
  raster::extract(elev_img, raster::extent(elev_img), buffer = 1000), 
  nrow = ncol(elev_img), ncol = nrow(elev_img))

raster::plot(elev_img, col=viridis(20))
plot(vn_mdw_boundary$geometry, add=TRUE, border="forestgreen")
plot(vn_lake_boundary$geometry, add=TRUE, border="blue", col="skyblue")
plot(vn_stream_ctrlpts$geometry, add=TRUE, col="slateblue", pch=17)

# save as a tif
# elev_tif <- raster::writeRaster(elev_img, "data/vn_elevation.tif", 
#                                 overwrite= TRUE)
# dim <- dim(elev_tif)

# convert to dataframe
elev_df <- raster::as.data.frame(elev_img, xy=TRUE, na.rm=T)
colnames(elev_df) <- c("x", "y", "value")


# GGPLOT ------------------------------------------------------------------
# plot
ggplot() +
  geom_tile(data=elev_df, aes(x=x, y=y, fill=value)) +
  geom_sf(data=vn_mdw_boundary, color="darkgreen", lwd=0.7, alpha=0.5) +
  geom_sf(data=vn_lake_boundary, fill="slateblue", color="darkblue",
          lwd=0.4, alpha=0.3) +
  geom_sf(data = wells_rtk_sf %>% filter(YYYY==2016), 
          aes(color = gw_rel_depth_m), size=5) +
  coord_sf(datum = 3310) +
  labs(title = "Van Norden Meadow", 
       subtitle = "GW Well Depth 2016") +
  theme_bw(base_family = "Roboto Condensed")+
  scale_color_viridis("GW Depth (m)", option = "A", direction = -1) 

# Make a GRID for SOAP Model ----------------------------------------------

# this is a reg grid across the shape...
# can make hex grid only WITHIN grid w/ square=F
grid_reg <- vn_mdw_boundary %>% 
  st_buffer(dist=-50) %>% 
  st_make_grid(cellsize = 100, what = "centers", square = T) %>%
  st_as_sf() %>% 
  mutate(os_x = st_coordinates(x)[,1],
         os_y = st_coordinates(x)[,2])

# can make hex grid only WITHIN grid w/ square=F
grid <- vn_mdw_boundary %>% 
  st_buffer(dist=-50) %>% 
  st_make_grid(cellsize = 100, what = "centers", square = F) %>%
  st_as_sf() %>% 
  mutate(os_x = st_coordinates(x)[,1],
         os_y = st_coordinates(x)[,2])

# make plots
ggplot() +
  geom_sf(data = grid_reg, alpha=.4, pch=21, fill="gray") +
  geom_sf(data = grid, pch=21, fill="orange") +
  geom_sf(data=vn_mdw_boundary, color="darkgreen", fill="transparent",lwd=0.7, alpha=0.5) +
  coord_sf(datum = 3310) +
  scale_fill_viridis_d("In/Out")+
  theme_bw(base_family = "Roboto Condensed")


# select points inside or outside
grid_in <- st_intersection(grid_reg, st_buffer(vn_mdw_boundary, dist=-50)) %>% 
  dplyr::select(os_x, os_y, x) %>% 
  mutate(f_in = 1)

grid_out <- grid_reg[!lengths(st_intersects(grid_reg, vn_mdw_boundary)), ] %>%
  mutate(f_in = 0)

# plot and join 
plot(vn_mdw_boundary$geometry, border="gray")
plot(grid_out$x, add=T, col="blue")
plot(grid_in$x, add=T, pch=16, col="maroon")

# join together
grids <- rbind(grid_in, grid_out)
 
# ggplot
ggplot() +
  geom_sf(data=vn_mdw_boundary, color="darkgreen",
          fill="transparent", 
          lwd=0.7, alpha=0.8) +
  geom_sf(data = grids, aes(fill=as.factor(f_in)), 
          alpha=.8, pch=21) +
  geom_sf(data = grid, fill="yellow", alpha=.8, pch=23) +
  coord_sf(datum = 3310) +
  scale_fill_viridis_d("In/Out", option = "A")+
  theme_bw(base_family = "Roboto Condensed")


# Set up Knots for Soap Model ----------------------------------------------

# see here:https://www.fromthebottomoftheheap.net/2016/03/27/soap-film-smoothers/

# get coords for boundary
crds <- st_coordinates(vn_mdw_boundary)[,1:2]
# make list form
bound <- list(list(x = crds[,1], y = crds[,2], 
                   f = rep(2075, nrow(crds))))
names(bound[[1]]) <- c("os_x", "os_y", "f")

# using sf add small buffer (can be manual like this or use above)
# knots <- st_intersection(grid, st_buffer(vn_mdw_boundary, dist = -50)) %>%
#   dplyr::select(os_x, os_y)

# use existing set inside boundary only!
knots <- grid

# plot check
plot(vn_mdw_boundary$geometry, border="darkgreen", lwd=2)
plot(grid$x, col=alpha("gray", 0.8), add=T)
plot(knots$x, pch=16, col=alpha("maroon", 0.9), cex=0.6, add=T)


#  Get Data to Model ------------------------------------------------------

# make topo data:
vn_rtk_topo_flag <- vn_rtk_topo %>% 
  st_transform(3310) %>% 
  filter(Code=="topo" | Code=="FLAG" | Code=="CHANNEL_EDGE") %>% 
  mutate(os_x = st_coordinates(.)[,1],
         os_y = st_coordinates(.)[,2]) %>% 
  st_intersection(., vn_mdw_boundary) %>% 
  #st_drop_geometry() %>% 
  dplyr::select(os_x, os_y, Code, elev_m)

plot(vn_mdw_boundary$geometry, border="gray", lwd=1.5)
plot(vn_rtk_topo_flag$geometry, add=T, col="maroon", cex=.3, pch=16)
#mapview(vn_mdw_boundary) + mapview(vn_rtk_topo_flag, col.regions="orange")


# Soap Model: TOPO --------------------------------------------------------

# mod for topo
m1 <- gam(-elev_m ~ s(os_x, os_y, bs = "so", xt = list(bnd = bound)),
          data = vn_rtk_topo_flag, method = "REML", knots = knots)

# mgcv plot
lims <- apply(crds, 2, range)
ylim <- lims[,2]
xlim <- lims[,1]

# soap plot
plot(m1, asp = 1, ylim = ylim, xlim = xlim, se = FALSE, 
     scheme = 2, main = "")
plot(vn_lake_boundary$geometry, 
     col=alpha("skyblue",.2), add=TRUE, border = "blue")

# soap plot
plot(m1, asp = 1, ylim = ylim, xlim = xlim, se = FALSE, 
     scheme = 0, main = "")
plot(vn_lake_boundary$geometry, 
     col=alpha("skyblue",.2), add=TRUE, border = "blue")


# Soap Model: Wells -------------------------------------------------------


# fix wells data
wells_14 <- wells_rtk_sf %>% 
  #filter(YYYY==2014) %>% 
  mutate(os_x = st_coordinates(.)[,1],
         os_y = st_coordinates(.)[,2]) %>% 
  st_drop_geometry() %>% 
  select(os_x, os_y, gw_rel_depth_m)

# mod for wells
m2 <- gam(-gw_rel_depth_m ~ s(os_x, os_y, bs = "so", xt = list(bnd = bound)),
          data = wells_14, method = "REML", knots = knots)

summary(m2)

# mgcv plot
lims <- apply(crds, 2, range)
ylim <- lims[,2]
xlim <- lims[,1]

plot(m1, asp = 1, ylim = ylim, xlim = xlim, se = FALSE, 
     scheme = 2, main = "")
plot(vn_lake_boundary$geometry, 
     col="skyblue", add=TRUE, border = "blue")


ggplot() +
  geom_tile(data = df, aes(x = Easting, y = Northing, fill = Groundwater)) +
  stat_contour(data = df, aes(x = Easting, y = Northing, z = Groundwater), 
               binwidth = 0.19, color="darkblue") +
  ggtitle(paste0("Depth to Groundwater: ", yr,"-",mon_name)) +
  xlab("") + ylab("") +
  scale_fill_gradient("GW\nDepth (m)", low = muted("blue"), high = "white",
                      breaks=seq(0,2.5,0.5), limits=c(0,2.5))+
  #scale_fill_viridis_c(name = "Groundwater (m)") +
  theme_bw(base_family = "Roboto Condensed") +
  geom_point(data=plotdfC, aes(x=Easting, y=Northing), pch=16) +
  geom_text(data=wells_rtk_sf, aes(x=Easting, y=Northing, label=wellID), hjust = -.3, family="Roboto Condensed")






# Adding MODIS NDVI -------------------------------------------------------

# https://ropensci.github.io/MODISTools/index.html
# https://verbe039.github.io/BFASTforAEO/
# https://terpconnect.umd.edu/~egurarie/teaching/MovementAtICCB2017/AnnotatingData.html