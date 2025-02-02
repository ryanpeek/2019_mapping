# 2019-12-10

# devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")
library(tidyverse)
library(lubridate)

# Set Token ---------------------------------------------------------------

# to get a token, go to website(https://eflows.ucdavis.edu/), 
# open console (F12) and type: localStorage.getItem('ff_jwt'), 
# copy token that comes back in quotes.

# library(usethis)
# edit_r_environ()
# add a line like this but substitute your token between the quotes:
# EFLOWS_TOKEN="atokenhere"

# restart R
# confirm it worked:
#Sys.getenv("EFLOWS_TOKEN", "")

ffcAPIClient::set_token(Sys.getenv("EFLOWS_TOKEN", "")) 
#ffcAPIClient::get_token()


# Updated Version ---------------------------------------------------------

daily_df <- get_usgs_gage_data(11427000)
results <- ffcAPIClient::get_ffc_results_for_usgs_gage(11427000)  
drh_plot <- ffcAPIClient::plot_drh(results)  # includes optional output_path argument to save to file automatically
drh_plot  # display the plot

# Get USGS Data for a Gage ------------------------------------------------

library(dataRetrieval)
library(tidyverse)
library(lubridate)
#library(sf)
#library(purrr)

# check what daily data is available for a gage ID
gageNo <- 11427000 # NF American

# check metadata (flow is 00060, daily mean is 00003)
(usgs_daily <- whatNWISdata(siteNumber=gageNo, service='dv', 
                            parameterCd = '00060', 
                            statCd="00003") %>%
    select(site_no, station_nm, dec_lat_va, dec_long_va, 
           dec_coord_datum_cd, alt_va, huc_cd, data_type_cd, 
           parm_cd, stat_cd, begin_date:count_nu) %>% 
    # rename cols
    rename(interval=data_type_cd, lat = dec_lat_va, lon=dec_long_va,
           huc8=huc_cd, site_id=site_no, date_begin=begin_date,
           date_end=end_date, datum=dec_coord_datum_cd, elev_m=alt_va) %>% 
    # add total year range
    mutate(yr_begin = year(date_begin),
           yr_end = year(date_end),
           yr_total = yr_end-yr_begin) )

# select and get flow data for station/param if over 10 years:
try(
  if(usgs_daily$yr_total>10){
    daily_df <- dataRetrieval::readNWISdv(siteNumbers=usgs_daily$site_id, parameterCd = "00060") %>% 
      dataRetrieval::addWaterYear() %>% 
      rename(flow=X_00060_00003, date=Date, gage=site_no,
             flow_flag=X_00060_00003_cd)
  } else("Less than 10 years of data...try again")
)

# RETRIEVE FF DATA --------------------------------------------------------

# get simple dataframe for API call
daily_df <- daily_df %>% #select(gage, date, flow) %>%  
  # date needs to be formatted as character "M/D/Y"
  mutate(date=format(as.Date(date),'%m/%d/%Y'))

# get data
results <- ffcAPIClient::get_ffc_results_for_df(df = daily_df, flow_field = "flow", date_field = "date")  

# Retrieve Results and Plot -----------------------------------------------

## get the DRH data as a data frame with percentiles for columns and days for rows
drh_data <- ffcAPIClient::get_drh(results) %>% 
  # add an index of days:
  mutate(days = seq(1:nrow(.)))

# fancy plot
ggplot() + 
  geom_ribbon(data=drh_data, aes(x=days, ymin=ten, ymax=ninty), fill="skyblue", alpha=0.3) +
  geom_ribbon(data=drh_data, aes(x=days, ymin=twenty_five, ymax=seventy_five), fill="slateblue", alpha=0.3) +
  geom_line(data=drh_data, aes(x=days, y=fifty), color="black", lwd=1.2) +
  theme_classic(base_family = "Roboto Condensed") +
  labs(title="Dimensionless Hydrograph", x="Julian Day", 
       y="Daily mean flow / Avg annual flow",
       caption="Daily mean flow with 10/90 percentiles (light blue), and 25/75 percentiles in purple")

ggsave(filename = "drhydrograph_nfa_ggplot.png", width = 7, height = 5, units = "in", dpi=300)
  


# GET MEAN ANNUAL METRICS -------------------------------------------------

dat_annual <- results$allYear %>% 
  unlist(recursive = FALSE) %>% 
  enframe %>%
  unnest(value, keep_empty=TRUE) %>% # keep null values and add NA
  mutate(wyears = rep(unlist(results$yearRanges), 3)) %>% 
  # now tidy
  separate(name,sep = "[:digit:]", into = "metric", remove=TRUE, extra="drop") %>% 
  pivot_wider(id_cols = "wyears", names_from = "metric", names_prefix = "ann_", values_from = c("value")) %>% 
  rename(ann_sd=ann_standard_deviations, ann_avg=ann_average_annual_flows,
         ann_cv = ann_coefficient_variations)

# plot
ggplot() + 
  geom_line(data=dat_annual, aes(x=wyears, y=ann_avg), color="darkblue", lwd=1, alpha=0.5) +
  geom_point(data=dat_annual, aes(x=wyears, y=ann_avg), fill="slateblue", pch=21, size=4) +
  theme_classic(base_family = "Roboto Condensed") +
  labs(title="Ann Mean Flow by Year", x="Water Year", 
       y="cfs")

# GET FF FALL METRICS ------------------------------------------------------

results$fall %>% map(list("timings"))

# this is slick
dat_fall <- results$fall %>% 
  unlist(recursive = FALSE) %>% 
  enframe %>%
  unnest(value, keep_empty=TRUE) %>% # keep null values and add NA
  mutate(wyears = rep(unlist(results$yearRanges),6)) %>% 
  #mutate(wyears = rep(c(1942:2020),6)) %>% 
  # now tidy
  separate(name,sep = "[:digit:]", into = "metric", remove=TRUE, extra="drop") %>% 
  pivot_wider(id_cols = "wyears", names_from = "metric", names_prefix = "fall_", values_from = c("value"))

#library(plotly)
#ggplotly(
ggplot() + 
  geom_segment(data=dat_fall, 
               aes(x=wyears, y=fall_timings_water, xend=wyears, yend=fall_wet_timings_water),
               arrow=arrow(type = "closed", length = unit(0.3,"cm")), 
               lineend = 'round',fill="gray50", alpha=0.5) +
  geom_point(data=dat_fall, aes(x=wyears, y=fall_wet_timings_water), fill="slateblue", pch=23, size=2.5) +
  geom_point(data=dat_fall, aes(x=wyears, y=fall_timings_water), fill="maroon", pch=21) +
  theme_classic(base_family = "Roboto Condensed") +
  labs(x="Water Year", y="Day of Water Year")
#)


daily_df %>% mutate(date=mdy(date)) %>% 
  filter(waterYear==2018) %>% 
ggplot(.) + 
  geom_line(aes(x=date, y=flow), color="darkblue", lwd=1, alpha=0.9) +
  theme_classic(base_family = "Roboto Condensed") +
  labs(title="Daily Mean Flow", x="", 
       y="Flow (cfs)") -> tstP1 

ggplotly(tstP1)

# check wateryeardays:wateRshedTools::dowy(ymd("1948-03-05"))
wateRshedTools::dowy(ymd("1947-10-17"))

# fall_timings_water = is start of first fall flush
# fall_magnitude = is mag of first fall flush 
# fall_durations = is duration (days) of first fall flush
# fall_wet_timings_water = is start of "wet" season
# fall_timings = is start of base flow period (and end of recession period)
# fall_wet_timings = ???? duration of wet/winter storm period? NO, had numbers at 355?


# GET FF SUMMER METRICS ------------------------------------------------------
names(results$summer)

# this is slick
dat_summer <- results$summer %>% 
  unlist(recursive = FALSE) %>% 
  enframe %>%
  unnest(value, keep_empty=TRUE) %>% # keep null values and add NA
  mutate(wyears = rep(unlist(results$yearRanges),7)) %>% 
  # now tidy
  separate(name,sep = "[:digit:]", into = "metric", remove=TRUE, extra="drop") %>% 
  pivot_wider(id_cols = "wyears", names_from = "metric", names_prefix = "summer_", values_from = c("value"))

ggplot() + 
    geom_segment(data=dat_summer, 
                 aes(x=wyears, y=fall_timings_water, xend=wyears, yend=fall_wet_timings_water),
                 arrow=arrow(type = "closed", length = unit(0.3,"cm")), 
                 lineend = 'round',fill="gray50", alpha=0.5) +
    geom_point(data=dat_fall, aes(x=wyears, y=fall_wet_timings_water), fill="slateblue", pch=23, size=2.5) +
    geom_point(data=dat_fall, aes(x=wyears, y=fall_timings_water), fill="maroon", pch=21) +
    theme_classic(base_family = "Roboto Condensed") +
    labs(x="Water Year", y="Day of Water Year")


# get wydays
wateRshedTools::dowy(ymd("2019-06-17"))
