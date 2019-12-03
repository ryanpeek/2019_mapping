# Query for CDEC info using sharpshootr

# https://ncss-tech.github.io/AQP/sharpshootR/CDEC.html

library(sharpshootR)
library(sf)
library(tidyverse)
library(mapview)
library(purrr)
library(lubridate)

# metrics using mean daily temps, then daily mean of each wateryear day
# timing of annual max
# magnitude of annual max
# annual mean

# GET SITES AND CLEAN -----------------------------------------------------

# load data
load("data/all_gages.rda")

# add an interval column:
all_gages <- all_gages %>% 
  mutate(interval = NA) %>% 
  select(site_id:date_end, interval, operator:data_source)

# filter to the cdec gages an drop sf class
cdec <- all_gages %>% filter(data_source=="CDEC") %>% 
  st_drop_geometry() %>% 
  select(site_id, interval, date_begin, date_end)

usgs <- all_gages %>% filter(data_source=="USGS") %>% 
  st_drop_geometry()
  
# DOWNLOAD METADATA W PURRR ------------------------------------------------


# use map to get the CDEC_StationInfo
cdec_info <- cdec %>% select(site_id) %>% 
  # these stations don't work, so filter out, check with #slice() 
  filter(!site_id %in% c("BND","FAR", "GRL","HEA", "MSD", 
                         "MST", "OBB", "RDB", "RDR", "SFJ", 
                         "SJP", "VON")) %>% 
  # run function to get metadata for each site_id
  mutate(sensor_data = map(site_id, 
                           ~CDEC_StationInfo(.x)[["sensor.meta"]])) %>% 
  # convert to dataframe (not listcol) & filter to only water temp data 
  unnest(cols=c(sensor_data)) %>% filter(sensor==25) %>% 
  # add a short abbrev col for interval
  mutate(interval_id = case_when(
    interval == "(hourly)" ~ "H",
    interval == "(event)" ~ "E",
    interval == "(daily)" ~ "D"))


# CLEAN METADATA -----------------------------------------------------------

# now clean to get begin/end dates, 
cdec_info <- cdec_info %>% 
  separate(period_of_record, sep = " to ", 
           into = c("date_begin", "date_end"), remove = FALSE) %>% 
  mutate(date_begin = gsub("/", replacement = "-", x = date_begin),
         date_end = gsub("/", replacement = "-", x = date_end),
         date_end = ifelse(date_end=="present", "10-01-2019", date_end),
         date_end = replace(date_end, site_id=="PTK", "10-01-2019"),
         date_begin = mdy(date_begin),
         date_end = mdy(date_end),
         yr_begin = year(date_begin),
         yr_end = year(date_end),
         yr_total = yr_end-yr_begin)

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=cdec_info, 
                 aes(x=site_id, ymin=yr_begin, ymax=yr_end, color=interval_id), position="dodge", show.legend = F) + coord_flip() + 
  labs(x="", y="") + 
  facet_grid(.~interval_id)+
  ggthemes::scale_color_pander()+
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)

ggsave(filename = "output/figures/cdec_gage_year_ranges_by_interval.pdf", 
       width=11, height = 9.5, units = "in", device = cairo_pdf)

# now filter to data where gages have >8 yrs of data (minimum req is 8.5)
cdec_filt <- cdec_info %>% filter(yr_total > 8)

# visualize the stations to see date ranges
ggplot() + 
  geom_linerange(data=cdec_filt, 
                 aes(x=site_id, ymin=yr_begin, ymax=yr_end, color=interval_id), position="dodge", show.legend = F) + coord_flip() + 
  labs(x="", y="") + 
  ggdark::dark_theme_bw(base_family = "Roboto Condensed", base_size = 8)+
  facet_grid(.~interval_id) +
  ggthemes::scale_color_pander()

ggsave(filename = "output/figures/cdec_gages_data_range_w_5yearsplus_interval.pdf", 
       width=11, height = 9.5, units = "in", device = cairo_pdf)

# DOWNLOAD DAILY DATA w PURRR ----------------------------------------------

# first filter to only "Daily" stations
cdec_daily <- cdec_filt %>% filter(interval_id=="D") %>% 
  select(site_id, sensor, interval_id, date_begin, date_end)

# use "pmap" function (parallel map) to loop through and get data
cdec_temps_day <- cdec_daily %>% 
  mutate(tempdata = pmap(cdec_daily, ~CDECquery(id=..1, sensor=..2, interval=..3, start=..4, end=..5))) 

# convert to dataframe (from list col)
cdec_temps_day <- cdec_temps_day %>% unnest(cols=c(tempdata))

# flag bad values:
cdec_temps_day <- cdec_temps_day %>% 
  mutate(flag=if_else(value<0 | value > 100, "Y", "N"))

# save out
save(cdec_temps_day, file = "data/cdec_temps_all_daily_filt8yr.rda")
save(cdec_info, file="data/cdec_stations_metadata_dateranges_112_stations.rda")

# DAILY PLOTS ---------------------------------------------------------------

# some prelim plots
ggplot() + 
  geom_point(data=cdec_temps_day %>% filter(flag=="N"), aes(x=water_day, y=value, color=year)) + ylim(c(30,90))+
  facet_grid(site_id~.)

ggplot() + 
  geom_line(data=cdec_temps_day %>% filter(flag=="N"), aes(x=datetime, y=value), color="dodgerblue", show.legend = F) + ylim(c(30,90))+ 
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y") +
  facet_grid(site_id~.) +
  labs(y="Temp (F)", x="") +
  ggdark::dark_theme_classic() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

ggsave(filename = "output/figures/daily_cdec_temps_example.png", dpi=300,
       width = 11, height = 8, units = "in")

# DOWNLOAD HOURLY DATA w PURRR ----------------------------------------------

load("data/cdec_temps_all_daily_filt8yr.rda")
load("data/cdec_stations_metadata_dateranges_112_stations.rda")

# now filter to data where gages have >8 yrs of data (minimum req is 8.5)
cdec_filt <- cdec_info %>% filter(yr_total > 8)

# get list of stations we have data for:
cdec_d_ids <- unique(cdec_temps_day$site_id)

# first filter to only "Hourly" stations
cdec_hourly <- cdec_filt %>% 
  filter(interval_id=="H", !site_id %in% cdec_d_ids) %>% 
  select(site_id, sensor, interval_id, date_begin, date_end)

# use "pmap" function (parallel map) to loop through and get data
cdec_temps_hr <- cdec_hourly %>% 
  mutate(tempdata = pmap(cdec_hourly, ~CDECquery(id=..1, sensor=..2, interval=..3, start=..4, end=..5))) 

# convert to dataframe (from list col)
cdec_temps_hr <- cdec_temps_hr %>% unnest(cols=c(tempdata))


# flag bad values:
cdec_temps_hr <- cdec_temps_hr %>% 
  mutate(flag=if_else(value<0 | value > 100, "Y", "N"))

# save out

# IT WORKED!! Save it immediately:
save(cdec_temps_hr, file = "data/cdec_temps_all_hourly_filt8yr.rda")

# save out stations we have data from:
cdec_h_ids <- unique(cdec_temps_hr$site_id)
station_list <- tibble("site_id"=cdec_d_ids, "interval"="D")
station_list2 <- tibble("site_id"=cdec_h_ids, "interval"="H")
station_list <- bind_rows(station_list, station_list2)
save(station_list, file="data/cdec_stations_completed.rda")

# DOWNLOAD EVENT DATA w PURRR ----------------------------------------------
  
load("data/cdec_stations_completed.rda")
load("data/cdec_stations_metadata_dateranges_112_stations.rda")

# now filter to data where gages have >8 yrs of data (minimum req is 8.5)
cdec_filt <- cdec_info %>% filter(yr_total > 8)

# first filter to only "Hourly" stations
cdec_event <- cdec_filt %>% 
  filter(interval_id=="E", !site_id %in% station_list$site_id) %>% 
  select(site_id, sensor, interval_id, date_begin, date_end)

# use "pmap" function (parallel map) to loop through and get data
cdec_temps_min <- cdec_event %>% 
  mutate(tempdata = pmap(cdec_event, ~CDECquery(id=..1, sensor=..2, interval=..3, start=..4, end=..5))) 

# convert to dataframe (from list col)
cdec_temps_min <- cdec_temps_min %>% unnest(cols=c(tempdata))

# flag bad values:
cdec_temps_min <- cdec_temps_min %>% 
  mutate(flag=if_else(value<0 | value > 100, "Y", "N"))

# IT WORKED!! Save it immediately:
save(cdec_temps_min, file = "data/cdec_temps_all_event_filt8yr.rda")

# save out stations we have data from:
cdec_e_ids <- unique(cdec_temps_min$site_id)
station_list3 <- tibble("site_id"=cdec_e_ids, "interval"="E")
station_list <- bind_rows(station_list, station_list3)
save(station_list, file="data/cdec_stations_completed.rda")
