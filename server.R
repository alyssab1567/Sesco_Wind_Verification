library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)
library(lubridate)
library(RSESCO)
library(ROracle)
library(geosphere)
library(tidyr)
library(scales)
library(DT)
setwd("P:/")

#Load data------
# Load Miso data from excel files
miso_wind <- list.files("/Weather/Forecasting Tools/Wind/Saved Forecasts/Miso/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(miso_wind)) {
  #print(i)
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

#Load Miso actual data from SESCO database
miso_verification <- query_actual_wind_mw(start = ymd('2018-02-11'), end = today(), region_id = 67529, timeZone = "US/Central") %>%
  mutate(date = floor_date(date, "minute"))
#use this line during day light ssavings time
#mutate(date = date - hours(1) - seconds(2))

colnames(miso_verification)[colnames(miso_verification) == "POWERMW"] <- "Actuals"
#colnames(miso_verification)[colnames(miso_verification) == "RTO_NAME"] <- "Iso"
miso_actuals <- miso_verification %>%
  select(date,Actuals)

#Join Miso actuals to the forecasted data
miso_all <- left_join(windmiso_actuals, miso_actuals) %>%
  group_by(date) %>%
  gather(type,mw,-date)
#----------------------------------------------------
#Miso Data manipulation
colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"

#----------------------------------------------------

#Load SPP data
SPP_wind <- list.files("/Weather/Forecasting Tools/Wind/Saved Forecasts/SPP/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(SPP_wind)) {
  #print(i)
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
# Get MISO actuals (verification)

#I dont think this code has the correct data for SPP but the other SPP code shows no data
SPP_verification <- query_actual_wind_mw(start = ymd('2018-02-11') , end = today(), region_id = 68, timeZone = "US/Central") %>% 
  mutate(date = date)
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
  gather(type,mw,-date)
  #filter(date>(start) & date<end)
#-------------------------------------------------------------------------------------
#SPP data manipulation
colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"




#Load ERCOT Data

# Ercot Actuals Pull ------------------------------------------------------
# Get ercot actuals (verification)
ercot_actuals <- query_wind_mw_ercot_actuals(start = ymd("2018-3-12"), end = today(), hourly = "yes", return_sql = F) %>% 
  select(-c(REGIONORZONEID)) %>%
  mutate(date = date + hours(1))

colnames(ercot_actuals)[colnames(ercot_actuals) == "MW"] <- "Actuals"


#Load Next Day files in
ercot_nextday <- list.files("P://Weather/ERCOT Wind/Saved Forecasts/Next Day/") %>%
  data.frame(files = .) %>%
  filter(grepl(".xlsx", files)) %>%
  pull(.)

output <- list()

for(i in 1:length(ercot_nextday)) {
  #print(i)
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


# 3 Tier Data Pull --------------------------------------------------------
# Gets 3 tier's ERCOT forecast. The data comes in individual farms, so must sum it all to get Forecast for ERCOT total 

regions <- find_region() %>% filter(CAID==9) 

farms <- find_wind_farms(center_longitude = -103.06708, center_latitude = 31.27669, max_distance_from_center = 1000) %>% 
  filter(REGIONID %in% regions$REGIONID) %>% 
  filter(OBSOLETEDAT == "9999-12-31 00:00:00")

timeZone <- "US/Central"

data3tier <- query_wind_DA_fcast_3tier_by_region(start = ymd('2018-02-11'), end = today(), 67509) %>% 
  select(-c(SAMPLETIMESTAMP,POWERMW24)) %>% 
  gather(hour,mw,-c(REGIONID,MARKETDAY,FARMID,FARMNAME))%>% 
  mutate(hour = as.numeric(gsub("POWERMW","", hour)), date = ymd_h(paste(MARKETDAY, hour, sep = ":"), tz = timeZone)) %>% 
  select(-c(REGIONID,FARMID,MARKETDAY))

data3tier_sum <- data3tier %>% 
  group_by(date) %>% 
  summarize(`3tier` = sum(mw)) %>% 
  filter(date>ymd('2018-02-11')) %>% 
  filter(!is.na(`3tier`)) 

# Join tables -------------------------------------------------------------
# Join the data together
ercot_all <- data3tier_sum %>% 
  group_by(date) %>% 
  left_join(.,E_nxtdaywnd) %>% 
  left_join(., ercot_actuals) %>% 
  gather(type,mw,-date)
  #filter(date>(start+1) & date<end)


#----------------------------------------------------------------------------------

setwd("C:/R Projects/Sesco_Wind_Verification")

server <- function(input, output, session) {
 
#MISO reactive data ------------------------------------------------------------------  
   selected_data_miso <- reactive({
    req(input$dateRange)
    validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
    miso_all%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_data_misoview <- reactive({
     miso_actuals%>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_windmiso_actuals <- reactive({
     windmiso_actuals%>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   miso_actuals_table <- reactive({
     miso_actuals%>%
       mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
       filter(date >= as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2]))
   })

   
#SPP reactive data ----------------------------------------------------------------------
   selected_data_spp <- reactive({
     req(input$dateRange)
     validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
     validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
     SPP_all%>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_data_sppview <- reactive({
     SPP_actuals %>% 
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_wind_forecasts <- reactive({
     Wind_forecasts%>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   SPP_actuals_table <- reactive({
     SPP_actuals%>%
       mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
       filter(date >= as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2]))
   })
   
   
#ERCOT reactive data calls---------------------------------------------------------------  
   #date > as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2])
   selected_data_ercot <-reactive({
     req(input$dateRange)
     validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
     validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
     ercot_all%>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_ercot_actuals <- reactive({
     ercot_actuals %>% 
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_E_nxtdaywnd <- reactive({
     E_nxtdaywnd %>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   selected_data3tier_sum <- reactive({
     data3tier_sum %>%
       filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
   })
   ercot_actuals_table <- reactive({
     ercot_actuals%>%
       mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
       filter(date >= as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2]))
   })
   
   
#Output for Miso tab ---------------------------------------------------------------------
  output$MISO <- renderPlot({
    
    ggplot(selected_data_miso(), aes(date,mw,col=type)) + geom_line(size=1)
    
  })
  output$summarymiso <- renderPrint({
    miso_MAE <- left_join(selected_windmiso_actuals(), selected_data_misoview()) %>% 
      group_by(date) %>% 
      mutate('3 Tier' = abs(Actuals -vaisala),
             Sesco = abs(Actuals - Sesco),
             MDA = abs(Actuals - MDA),
             WSI = abs(Actuals - WSI)) %>%  
      select(date, Sesco, '3 Tier', WSI, MDA) %>% 
      gather(type,mw,-date) %>% 
      #filter(date>start & date<end) %>% 
      filter(!is.na(mw)) %>% 
      mutate(hourgroups = case_when(hour(date) %in% c(0:5)~"1: Early AM (0-5)",
                                    hour(date) %in% c(6:10)~"2: Morning (6-10)",
                                    hour(date) %in% c(11:13)~"3: Mid-day (11-13)",
                                    hour(date) %in% c(14:19)~"4: Peak (14-19)",
                                    hour(date) %in% c(20:23)~"5: Late PM (20-23)")) %>% 
      # group_by(type) %>% 
      # summarise(MAE = mean(mw)) %>% 
      identity()  
    
    aggregate(miso_MAE[,3], list(miso_MAE$type), mean)
  })
  output$misomae <- renderPlot({
    miso_MAE <- left_join(selected_windmiso_actuals(), selected_data_misoview()) %>% 
      group_by(date) %>% 
      mutate('3 Tier' = abs(Actuals -vaisala),
             Sesco = abs(Actuals - Sesco),
             MDA = abs(Actuals - MDA),
             WSI = abs(Actuals - WSI)) %>%  
      select(date, Sesco, '3 Tier', WSI, MDA) %>% 
      gather(type,mw,-date) %>% 
      #filter(date>start & date<end) %>% 
      filter(!is.na(mw)) %>% 
      mutate(hourgroups = case_when(hour(date) %in% c(0:5)~"1: Early AM (0-5)",
                                    hour(date) %in% c(6:10)~"2: Morning (6-10)",
                                    hour(date) %in% c(11:13)~"3: Mid-day (11-13)",
                                    hour(date) %in% c(14:19)~"4: Peak (14-19)",
                                    hour(date) %in% c(20:23)~"5: Late PM (20-23)")) %>% 
      # group_by(type) %>% 
      #summarise(MAE = mean(mw) %>% 
      identity()
    
    daytimegraph <- aggregate(miso_MAE[,3], list(time = miso_MAE$hourgroups, type = miso_MAE$type), mean)
    
    ggplot(daytimegraph,aes(type, mw, fill=type)) +
      geom_col() +
      facet_wrap(~time) +
      ggtitle("MISO - Mean Absolute Error of time blocks")
  })
  output$viewmiso <- DT::renderDataTable({
    DT::datatable(miso_actuals_table())
  })
  
  
  #Output for SPP tab -------------------------------------------------------------------------------
  output$SPP <- renderPlot({
    
    ggplot(selected_data_spp(), aes(date,mw,col=type)) + geom_line(size=1)
    
  })
  output$summaryspp <- renderPrint({
    SPP_MAE <- left_join(selected_wind_forecasts(), selected_data_sppview()) %>% 
      group_by(date) %>% 
      mutate('3 Tier' = abs(Actuals - vaisala),
             Sesco = abs(Actuals - Sesco),
             WSI = abs(Actuals - WSI),
             SPP = abs(Actuals - SPP)) %>%  
      select(date, Sesco, '3 Tier', WSI, SPP) %>% 
      gather(type,mw,-date) %>% 
      #filter(date>start & date<end) %>% 
      filter(!is.na(mw)) %>% 
      mutate(hourgroups = case_when(hour(date) %in% c(0:5)~"1: Early AM (0-5)",
                                    hour(date) %in% c(6:10)~"2: Morning (6-10)",
                                    hour(date) %in% c(11:13)~"3: Mid-day (11-13)",
                                    hour(date) %in% c(14:19)~"4: Peak (14-19)",
                                    hour(date) %in% c(20:23)~"5: Late PM (20-23)")) %>% 
      # group_by(type) %>% 
      # summarise(MAE = mean(mw)) %>% 
      identity()
    
    aggregate(SPP_MAE[,3], list(SPP_MAE$type), mean)
    
  })
  output$sppmae <- renderPlot({
      SPP_MAE <- left_join(selected_wind_forecasts(), selected_data_sppview()) %>% 
        group_by(date) %>% 
        mutate('3 Tier' = abs(Actuals - vaisala),
               Sesco = abs(Actuals - Sesco),
               WSI = abs(Actuals - WSI),
               SPP = abs(Actuals - SPP)) %>%  
        select(date, Sesco, '3 Tier', WSI, SPP) %>% 
        gather(type,mw,-date) %>% 
        #filter(date>start & date<end) %>% 
        filter(!is.na(mw)) %>% 
        mutate(hourgroups = case_when(hour(date) %in% c(0:5)~"1: Early AM (0-5)",
                                      hour(date) %in% c(6:10)~"2: Morning (6-10)",
                                      hour(date) %in% c(11:13)~"3: Mid-day (11-13)",
                                      hour(date) %in% c(14:19)~"4: Peak (14-19)",
                                      hour(date) %in% c(20:23)~"5: Late PM (20-23)")) %>% 
        # group_by(type) %>% 
        # summarise(MAE = mean(mw)) %>% 
        identity()
      
      daytimegraph <- aggregate(SPP_MAE[,3], list(time = SPP_MAE$hourgroups, type = SPP_MAE$type), mean)
      
      ggplot(daytimegraph,aes(type, mw, fill=type)) +
        geom_col() +
        facet_wrap(~time) +
        ggtitle("SPP - Mean Absolute Error of time blocks")
  })
  output$viewspp <- DT::renderDataTable({
    DT::datatable(SPP_actuals_table())
  })
  
  
  #Output for ercot tab ---------------------------------------------------------------------------
  output$ERCOT <- renderPlot({
    
    ggplot(selected_data_ercot(), aes(date,mw,col=type)) + geom_line(size=1)
    
  })
  
  output$summaryercot <- renderPrint({
    ercot_MAE <- selected_data3tier_sum() %>% 
      group_by(date) %>% 
      left_join(.,selected_E_nxtdaywnd()) %>% 
      left_join(.,selected_ercot_actuals()) %>% 
      mutate('3tier' = abs(Actuals -`3tier`),
             SESCO = abs(Actuals - SESCO),
             ERCOT = abs(Actuals - ERCOT)) %>%  
      select(date, '3tier', SESCO, ERCOT) %>% 
      gather(type,mw,-date) %>% 
      #filter(date>start & date<end) %>% 
      filter(!is.na(mw)) %>% 
      mutate(hourgroups = case_when(hour(date) %in% c(0:4)~"1: Null period (0-4)",
                                    hour(date) %in% c(5:8)~"2: Morning (5-8)",
                                    hour(date) %in% c(9:11)~"3: Morning dip (9-11)",
                                    hour(date) %in% c(12:16)~"4: Afternoon (12-16)",
                                    hour(date) %in% c(17:19)~"5: Evening Peak (17-19)",
                                    hour(date) %in% c(20:23)~"6: Late PM (20-23)")) %>% 
      # group_by(type) %>% 
      # summarise(MAE = mean(mw)) %>% 
      identity()
    
    aggregate(ercot_MAE[,3], list(ercot_MAE$type), mean)
  })
  
  output$ercotmae <- renderPlot({
    ercot_MAE <- selected_data3tier_sum() %>% 
      group_by(date) %>% 
      left_join(.,selected_E_nxtdaywnd()) %>% 
      left_join(.,selected_ercot_actuals()) %>% 
      mutate('3tier' = abs(Actuals -`3tier`),
             SESCO = abs(Actuals - SESCO),
             ERCOT = abs(Actuals - ERCOT)) %>%  
      select(date, '3tier', SESCO, ERCOT) %>% 
      gather(type,mw,-date) %>% 
      #filter(date>start & date<end) %>% 
      filter(!is.na(mw)) %>% 
      mutate(hourgroups = case_when(hour(date) %in% c(0:4)~"1: Early Morning (0-4)",
                                    hour(date) %in% c(5:8)~"2: Morning (5-8)",
                                    hour(date) %in% c(9:11)~"3: Morning dip (9-11)",
                                    hour(date) %in% c(12:16)~"4: Afternoon (12-16)",
                                    hour(date) %in% c(17:19)~"5: Evening Peak (17-19)",
                                    hour(date) %in% c(20:23)~"6: Late PM (20-23)")) %>% 
      # group_by(type) %>% 
      # summarise(MAE = mean(mw)) %>% 
      identity()
    
    # Takes the mean absolute error for each specific time frame. 
    daytimegraph <- aggregate(ercot_MAE[,3], list(time = ercot_MAE$hourgroups, type = ercot_MAE$type), mean)
    
    # Plot the Mean Absolute error for each time of day 
    ggplot(daytimegraph,aes(type, mw, fill=type)) +
      geom_col() +
      facet_wrap(~time) +
      ggtitle("Mean Absolute Error of time blocks")
    
  })
  
  output$viewercot <- DT::renderDataTable({
    DT::datatable(ercot_actuals_table())
  })
  
}
