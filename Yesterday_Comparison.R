library(dplyr)
library(xlsx)
library(lubridate)
library(RSESCO)
library(ROracle)
library(geosphere)
library(ggplot2)
library(tidyr)
library(dplyr)

# Looks at the wind verification from the last two days 

# Dates -------------------------------------------------------------------
# How many days do you want to look back at. Currently the default is to look at the past 2 days. 
# Always put one more day then what you want. For example if you wan tthe last two days, put 0-3. 
end <- today() - days(0)
start <- today() - days(2)


# Sesco Data Pull ---------------------------------------------------------
## Gets SESCO's ERCOT wind forecast, which also includes ERCOT's forecast at the time we created our. The data is in local time and
## all the files in the folder are pulled. 

ercot_nextday <- list.files("P://Weather/ERCOT Wind/Saved Forecasts/Next Day/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(ercot_nextday)) {
  print(i)
  date <- substr(ercot_nextday[i], 10, 19)
  x <- read.xlsx(sprintf("P://Weather/ERCOT Wind/Saved Forecasts/Next Day/%s", ercot_nextday[i]),sheetIndex = 1, 
                 startRow = 1, endRow = 26, colIndex = 1:4) %>% 
    mutate(date = date)
  output[[paste(gsub(".rds" , replacement = "", ercot_nextday[i]))]] <- x
}

timezones <- data.frame(tz = OlsonNames())
E_nxtdaywnd <- do.call(rbind, output) %>% 
  mutate(date = ymd_h(paste0(date,":",He),tz = "US/Central") ) %>% 
  select(date, ERCOT, SESCO) 



# Sesco Data Pull - only one day ------------------------------------------
# This grabs just the previous day's files instead all of them. There is currebtly an error when running this. 

# ercot_nextday <- list.files("P://Weather/ERCOT Wind/Saved Forecasts/Next Day/") %>%
#   data.frame(files = .) %>%
#   filter(grepl(".xlsx", files)) %>%
#   mutate(day = ymd(substr(files,10,19))) %>%
#   filter(day>=start) %>%
#   pull(.)
# 
# output <- list()
# 
# for(i in 1:length(ercot_nextday)) {
#   print(i)
#   date <- substr(ercot_nextday[i], 10, 19)
#   x <- read.xlsx(sprintf("P://Weather/ERCOT Wind/Saved Forecasts/Next Day/%s", ercot_nextday[i]),sheetIndex = 1,
#                  startRow = 1, endRow = 26, colIndex = 1:4) %>%
#     mutate(date = day)
#   output[[paste(gsub(".rds" , replacement = "", ercot_nextday[i]))]] <- x
# }
# 
# timezones <- data.frame(tz = OlsonNames())
# E_nxtdaywnd <- do.call(rbind, output) %>%
#   mutate(date = ymd_h(paste0(date,":",He),tz = "US/Central") ) %>%
#   select(date, ERCOT, SESCO)


# 3 Tier Data Pull --------------------------------------------------------
# Gets 3 tier's ERCOT forecast. The data comes in individual farms, so must sum it all to get Forecast for ERCOT total 

regions <- find_region() %>% filter(CAID==9) 

farms <- find_wind_farms(center_longitude = -103.06708, center_latitude = 31.27669, max_distance_from_center = 1000) %>% 
  filter(REGIONID %in% regions$REGIONID) %>% 
  filter(OBSOLETEDAT == "9999-12-31 00:00:00")

timeZone <- "US/Central"

data3tier <- query_wind_DA_fcast_3tier_by_region(start, end, 67509) %>% 
  select(-c(SAMPLETIMESTAMP,POWERMW24)) %>% 
  gather(hour,mw,-c(REGIONID,MARKETDAY,FARMID,FARMNAME))%>% 
  mutate(hour = as.numeric(gsub("POWERMW","", hour)), date = ymd_h(paste(MARKETDAY, hour, sep = ":"), tz = timeZone)) %>% 
  select(-c(REGIONID,FARMID,MARKETDAY))

data3tier_sum <- data3tier %>% 
  group_by(date) %>% 
  summarize(`3tier` = sum(mw)) %>% 
  filter(date>start) %>% 
  filter(!is.na(`3tier`)) 





# Ercot Actuals Pull ------------------------------------------------------
# Get ercot actuals (verification)

ercot_actuals <- query_wind_mw_ercot_actuals(start = start, end = end, hourly = "yes", return_sql = F) %>% 
  select(-c(REGIONORZONEID)) %>%
  mutate(date = date + hours(1))

colnames(ercot_actuals)[colnames(ercot_actuals) == "MW"] <- "Actuals"



# Join tables -------------------------------------------------------------
# Join the data together
ercot_all <- data3tier_sum %>% 
  group_by(date) %>% 
  left_join(.,E_nxtdaywnd) %>% 
  left_join(., ercot_actuals) %>% 
  gather(type,mw,-date) %>% 
  filter(date>(start+1) & date<end)


# Plot curves -------------------------------------------------------------
#plot ercot fsct, sesco fcst, 3 tier fcst and the verification 
ggplot(ercot_all,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("Forecasts vs Actuals")


# Calculate Forecast Difference  ------------------------------------------
# Get the error for each forecast 
ercot_dif <- data3tier_sum %>% 
  group_by(date) %>% 
  left_join(.,E_nxtdaywnd) %>% 
  left_join(., ercot_actuals) %>% 
  mutate(dif3tier = Actuals -`3tier`,
         difSESCO = Actuals - SESCO,
         difERCOT = Actuals - ERCOT,) %>% 
  select(date, dif3tier, difSESCO, difERCOT) %>% 
  gather(type,mw,-date) %>% 
  filter(date>start & date<end)


# Error Plot --------------------------------------------------------------
# Plot the error for the ercot fsct, sesco fcst and 3 tier fcst 
ggplot(ercot_dif,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("Error (Actual - Forecast)")



# Mean absolute (MAE) -----------------------------------------------------
# Calulate the absolute error and then plot it 
ercot_MAE <- data3tier_sum %>% 
  group_by(date) %>% 
  left_join(.,E_nxtdaywnd) %>% 
  left_join(., ercot_actuals) %>% 
  mutate('3tier' = abs(Actuals -`3tier`),
         SESCO = abs(Actuals - SESCO),
         ERCOT = abs(Actuals - ERCOT)) %>%  
  select(date, '3tier', SESCO, ERCOT) %>% 
  gather(type,mw,-date) %>% 
  filter(date>start & date<end) %>% 
  filter(!is.na(mw)) %>% 
  mutate(hourgroups = case_when(hour(date) %in% c(0:5)~"1: Early AM (0-5)",
                                hour(date) %in% c(6:10)~"2: Morning (6-10)",
                                hour(date) %in% c(11:13)~"3: Mid-day (11-13)",
                                hour(date) %in% c(14:19)~"4: Peak (14-19)",
                                hour(date) %in% c(20:23)~"5: Late PM (20-23)")) %>% 
  # group_by(type) %>% 
  # summarise(MAE = mean(mw)) %>% 
  identity()

# Absolute error graph 
ggplot(ercot_MAE,aes(date,mw,col=type)) + 
  geom_line() +
  ggtitle("Absolute Error")

# Takes average error value for the each forecast 
aggregate(ercot_MAE[,3], list(ercot_MAE$type), mean)

# Takes the mean absolute error for each specific time frame. 
daytimegraph <- aggregate(ercot_MAE[,3], list(time = ercot_MAE$hourgroups, type = ercot_MAE$type), mean)

# Plot the Mean Absolute error for each time of day 
ggplot(daytimegraph,aes(type, mw, fill=type)) +
  geom_col() +
  facet_wrap(~time) +
  ggtitle("Mean Average Error of time blocks")

