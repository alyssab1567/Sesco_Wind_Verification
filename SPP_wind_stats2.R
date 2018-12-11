library(dplyr)
library(readxl)
library(lubridate)
library(RSESCO)
library(ROracle)
library(geosphere)
library(ggplot2)
library(tidyr)
setwd("P:/")

# Looks at the wind verification from yesterday

# Dates -------------------------------------------------------------------
# How many days do you want to look back at. Currently the default is to look at the past day. 
# Always put one more day then what you want. For example if you wan the last two days, put 0-3. 
end <- today() - days(0)
start <- today() - days(1)

SPP_wind <- list.files("/Weather/Forecasting Tools/Wind/Saved Forecasts/SPP/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(SPP_wind)) {
  print(i)
  date <- paste(substr(SPP_wind[i], 18, 19),substr(SPP_wind[i], 14, 15), sep = "-")
  date <- paste(date, substr(SPP_wind[i], 16, 17), sep = "-")
  date <- paste0("20", date)
  x <- read_excel(sprintf("/Weather/Forecasting Tools/Wind/Saved Forecasts/SPP/%s", SPP_wind[i]) ,sheet = 2, 
                 range = "B2:K26" , col_names = TRUE) %>% 
    mutate(date = date)
  output[[paste(gsub(".rds" , replacement = "", SPP_wind[i]))]] <- x
}

timezones <- data.frame(tz = OlsonNames())

Wind_forecasts <- do.call(rbind, output) %>% 
  mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
  select(date, Sesco, '3 Tier', WSI, SPP)

#SPP Actuals pull------------------------------------------------------
# Get SPP actuals (verification)

#I dont think this code has the correct data for SPP but the other SPP code shows no data
SPP_verification <- query_actual_wind_mw(start = start , end = end, region_id = 68, timeZone = "US/Central") %>% 
  mutate(date= date)
#use the following line during day light savings time
#mutate(date = date - hours(1))


colnames(SPP_verification)[colnames(SPP_verification) == "POWERMW"] <- "Actuals"
SPP_actuals <- SPP_verification %>%
  filter(., grepl(":00:00",date)) %>%
  select(date, Actuals)
#SPP_actuals <- filter(SPP_actuals, grepl(":00:00", date))

  
#Join tables -------------------------------------------------------------
#Join the data together
SPP_all <- left_join(Wind_forecasts, SPP_actuals) %>%
  group_by(date) %>%
  gather(type,mw,-date) %>%
  filter(date>(start) & date<end)

# Plot curves -------------------------------------------------------------
#plot ercot fsct, sesco fcst, 3 tier fcst and the verification 
ggplot(SPP_all,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("SPP Forecasts vs Actuals")

colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"

# Calculate Forecast Difference  ------------------------------------------
# Get the error for each forecast 
SPP_dif <- left_join(Wind_forecasts, SPP_actuals) %>%
  group_by(date) %>% 
  mutate(difSesco = as.numeric(Actuals) - as.numeric(Sesco),
         dif3tier = as.numeric(Actuals) - as.numeric(vaisala),
         difWSI = as.numeric(Actuals) - as.numeric(WSI),
         difSPP = as.numeric(Actuals) - as.numeric(SPP)) %>% 
  select(date, difSesco, dif3tier, difWSI, difSPP) %>% 
  gather(type,mw,-date) %>% 
  filter(date>start & date<end)

# Error Plot --------------------------------------------------------------
# Plot the error for the ercot fsct, sesco fcst and 3 tier fcst 
ggplot(SPP_dif,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("SPP Error (Actual - Forecast)")

# Mean absolute (MAE) -----------------------------------------------------
# Calulate the absolute error and then plot it 
SPP_MAE <- left_join(Wind_forecasts, SPP_actuals) %>% 
  group_by(date) %>% 
  mutate('3 Tier' = abs(Actuals - vaisala),
         Sesco = abs(Actuals - Sesco),
         WSI = abs(Actuals - WSI),
         SPP = abs(Actuals - SPP)) %>%  
  select(date, Sesco, '3 Tier', WSI, SPP) %>% 
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
ggplot(SPP_MAE,aes(date,mw,col=type)) + 
  geom_line() +
  ggtitle("SPP - Absolute Error")

# Takes average error value for the each forecast 
aggregate(SPP_MAE[,3], list(SPP_MAE$type), mean)

# Takes the mean absolute error for each specific time frame. 
daytimegraph <- aggregate(SPP_MAE[,3], list(time = SPP_MAE$hourgroups, type = SPP_MAE$type), mean)

# Plot the Mean Absolute error for each time of day 
ggplot(daytimegraph,aes(type, mw, fill=type)) +
  geom_col() +
  facet_wrap(~time) +
  ggtitle("SPP - Mean Average Error of time blocks")

