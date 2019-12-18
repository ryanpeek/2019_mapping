# 2019-12-10

#devtools::install_github('ceff-tech/ffc_api_client/ffcAPIClient')
library("ffcAPIClient")

# Set Token ---------------------------------------------------------------

#api_token <- # go to website(https://eflows.ucdavis.edu/), open console (F12) and type: localStorage.getItem('ff_jwt'), copy token that comes back in quotes.

#ffcAPIClient::set_token(api_token) 
#ffcAPIClient::get_token() 


# TEST RUN ----------------------------------------------------------------

test_data <- example_gagedata(gages = "11427000")  # just get some fake gage data - based on Daniel Philippus' code

results <- get_ffc_results_for_df(test_data)

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

# get simple dataframe
daily_df_simple <- daily_df %>% select(gage, date, flow) %>%  
  # date needs to be formatted as a factor????
  mutate(date=format(as.Date(date),'%m/%d/%Y'))

# get data
results <- ffcAPIClient::get_ffc_results_for_df(daily_df_simple)  

# Retrieve Results and Plot -----------------------------------------------

## get the DRH data as a data frame with percentiles for columns and days for rows
drh_data <- ffcAPIClient::get_drh(results) %>% 
  # add an index of days:
  mutate(days = seq(1:nrow(.)))

plot(drh_data$seventy_five, type="l")  # plot the seventy-fifth percentile DRH

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
  

# Get Metrics? ------------------------------------------------------------

