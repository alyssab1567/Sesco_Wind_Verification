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

miso_wind <- list.files("/Weather/Forecasting Tools/Wind/Saved Forecasts/Miso/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(miso_wind)) {
  print(i)
  date <- paste(substr(miso_wind[i], 19, 20),substr(miso_wind[i], 15, 16), sep = "-")
  date <- paste(date, substr(miso_wind[i], 17, 18), sep = "-")
  date <- paste0("20", date)
  x <- read_excel(sprintf("/Weather/Forecasting Tools/Wind/Saved Forecasts/Miso/%s", miso_wind[i]) ,sheet = 2, 
                  range = "B2:J26" , col_names = TRUE) %>% 
    mutate(date = date)
  output[[paste(gsub(".rds" , replacement = "", miso_wind[i]))]] <- x
}

timezones <- data.frame(tz = OlsonNames())
windmiso_actuals <- do.call(rbind, output) %>% 
  mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
  select(date, Sesco, '3 Tier', MDA, WSI)


#Miso Actuals pull------------------------------------------------------
# Get MISO actuals (verification)

miso_verification <- query_actual_wind_mw(start = start, end = end, region_id = 67529, timeZone = "US/Central") %>%
  mutate(date = floor_date(date, "minute"))
#use this line during day light savings time
#mutate(date = date - hours(1) - seconds(2))

colnames(miso_verification)[colnames(miso_verification) == "POWERMW"] <- "Actuals"
miso_actuals <- miso_verification %>%
  select(date, Actuals)


#Join tables -------------------------------------------------------------
#Join the data together
miso_all <- left_join(windmiso_actuals, miso_actuals) %>%
  group_by(date) %>%
  gather(type,mw,-date) %>%
  filter(date>(start) & date<end)

# Plot curves -------------------------------------------------------------
#plot ercot fsct, sesco fcst, 3 tier fcst and the verification 
ggplot(miso_all,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("Miso Forecasts vs Actuals")

colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"

# Calculate Forecast Difference  ------------------------------------------
# Get the error for each forecast 
miso_dif <- left_join(windmiso_actuals, miso_actuals) %>%
  group_by(date) %>% 
  mutate(difSesco = as.numeric(Actuals) - as.numeric(Sesco),
         dif3tier = as.numeric(Actuals) - as.numeric(vaisala),
         difMDA = as.numeric(Actuals) - as.numeric(MDA),
         difWSI = as.numeric(Actuals) - as.numeric(WSI)) %>% 
  select(date, difSesco, dif3tier, difMDA, difWSI) %>% 
  gather(type,mw,-date) %>% 
  filter(date>start & date<end)

# Error Plot --------------------------------------------------------------
# Plot the error for the ercot fsct, sesco fcst and 3 tier fcst 
ggplot(miso_dif,aes(date,mw,col=type)) + 
  geom_line()+
  ggtitle("MISO Error (Actual - Forecast)")

# Mean absolute (MAE) -----------------------------------------------------
# Calulate the absolute error and then plot it 
miso_MAE <- left_join(windmiso_actuals, miso_actuals) %>% 
  group_by(date) %>% 
  mutate('3 Tier' = abs(Actuals -vaisala),
    Sesco = abs(Actuals - Sesco),
    MDA = abs(Actuals - MDA),
    WSI = abs(Actuals - WSI)) %>%  
  select(date, Sesco, '3 Tier', WSI, MDA) %>% 
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
ggplot(miso_MAE,aes(date,mw,col=type)) + 
  geom_line() +
  ggtitle("MISO - Absolute Error")

# Takes average error value for the each forecast 
aggregate(miso_MAE[,3], list(miso_MAE$type), mean)

# Takes the mean absolute error for each specific time frame. 
daytimegraph <- aggregate(miso_MAE[,3], list(time = miso_MAE$hourgroups, type = miso_MAE$type), mean)

# Plot the Mean Absolute error for each time of day 
ggplot(daytimegraph,aes(type, mw, fill=type)) +
  geom_col() +
  facet_wrap(~time) +
  ggtitle("MISO - Mean Average Error of time blocks")

