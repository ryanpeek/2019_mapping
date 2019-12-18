# stupid prop 1 maps stuff

library(tidyverse)
library(mapview)
library(sf)
library(USAboundaries)
library(hrbrthemes)

# sites
sites_kml <- st_read("data/prop1_CDFW_genoscape_sites_final.kml") %>% 
  mutate(lon=map_dbl(geometry, ~st_coordinates(.x)[[1]]),
         lat=map_dbl(geometry, ~st_coordinates(.x)[[2]])) %>% 
  select(Name, lon, lat) %>% st_transform(4326) %>% st_drop_geometry() %>% 
  st_as_sf(coords=c("lon","lat"), remove=FALSE, crs=4326)

# counties
counties <- USAboundaries::us_counties(states="CA") %>% 
  st_transform(4326)

# ca_house_districts
library(tigris)
ca_house <- state_legislative_districts(state = "California", house = "lower", cb = TRUE, class="sf", year=2018)
ca_house <- ca_house %>% st_transform(4326)
ca_house_crop <- ca_house[sites_kml,]

# ca_senate districts
ca_sen <- state_legislative_districts(state = "California", house = "upper", cb = TRUE, class="sf", year=2018)
ca_sen <- ca_sen %>% st_transform(4326)
ca_sen_crop <- ca_sen[sites_kml,]
#plot(ca_house_crop$geometry)
#plot(sites_kml$geometry, col="blue",add=T)

# add attributes of house to points:
sites_kml <- st_join(sites_kml, left=FALSE, ca_house_crop["NAME"]) %>% 
  rename(ca_house=NAME)
sites_kml <- st_join(sites_kml, left=FALSE, ca_sen_crop["NAME"]) %>% 
  rename(ca_sen=NAME)
sites_kml <- st_join(sites_kml, left=FALSE, counties[c("name","geoid")])

# ggplot
ggplot() + 
  geom_sf(data=ca_house_crop, fill="darkgray",color = "white", size = 0.25, show.legend = F) +
  geom_sf(data=ca_sen_crop, fill="transparent",color = "purple", size = 0.5, show.legend = F) +
  geom_sf_text(data=ca_sen_crop, aes(label=NAME), color = "purple", size = 3) +
  geom_sf(data=sites_kml, pch=21, fill="orange")+
  geom_sf_text(data=sites_kml, aes(label=ca_house), size=3, color="white", nudge_y=0.1, check_overlap=TRUE) +
  coord_sf(datum = NA) +
  labs(x = NULL, y = NULL,
       title = "2018 CA Borders",
       caption = "Data source: {tigris}\nhttps://github.com/ryanpeek/2019_mapping 30DayMapChallenge"
  ) + theme_ft_rc()
  

# Mapview -----------------------------------------------------------------

mapview(sites_kml, col.regions="orange") + mapview(ca_house, col.regions=NA) +
  mapview(counties, col.regions=NA, col="green")



# Save out ----------------------------------------------------------------


# write to csv

write_csv(sites_kml, path = "data/prop1_CDFW_genoscape_sites.csv")
