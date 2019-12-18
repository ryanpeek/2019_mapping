# cool radar thing, credit to hrbrmstr tweet:
# CODE: https://paste.sr.ht/~hrbrmstr/c63d38b7bdea4385e165940f451198e122c69fa4
# TWEET: https://twitter.com/hrbrmstr/status/1201975946015858697

library(rvest)
library(magick)
library(glue)
library(tidyverse)

# see here for list of radar sites: https://radar.weather.gov/ridge/
# then click on one and look at upper left side of screen for three letter abbrev:
# e.g., Sac: DAX, Hanford: HNX, WestCA: MUX

animate_radar <- function(station = "DAX") {
  
  county_url <- "https://radar.weather.gov/Overlays/County/Short/{station}_County_Short.gif"
  
  county <- image_read(glue(county_url))
  
  ir <- possibly(image_read, NULL)
  
  frames_dir_url <- "https://radar.weather.gov/ridge/RadarImg/N0R/{station}/"
  
  httr::GET(url = glue(frames_dir_url)) %>% 
    httr::content() %>% 
    html_nodes(xpath = glue(".//a[contains(@href, '{station}_')]")) %>% 
    html_attr("href") %>% 
    sprintf(glue("https://radar.weather.gov/ridge/RadarImg/N0R/{station}/%s"), .) %>% 
    map(ir) %>% 
    compact() %>% 
    do.call(c, .) -> radar_imgs
  
  image_background(county, "black", flatten = TRUE) %>% 
    image_composite(radar_imgs) %>% 
    image_animate() -> gif
  
  gif
  
}


animate_radar("MUX")
animate_radar("DAX")
animate_radar("HNX")
animate_radar("ESX")


## library
library(rradar)
library(tidyverse)

filter(stations, state == "California")

animate_radar("MUX")
