library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(writexl)
library(xlsx)
library(lubridate)
library(RSESCO)
library(ROracle)
library(geosphere)
library(tidyr)
library(scales)
library(DT)

Sys.setenv("USER" = "sescouser")

# #Load data------
# # Load Miso data from master excel file
# windmiso_actuals <- read_excel("/MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
#                                range = cell_cols("A:E") , col_names = TRUE) 
# 
# #windmiso_actuals$date <- as.POSIXct(windmiso_actuals$date, format = "%Y-%m-%d %H:%M:%S")
# windmiso_actuals <- windmiso_actuals %>%
#   mutate(date = ymd_hms(date, tz = "US/Central")) %>%
#   mutate(date = date - hours(6))


# #Load Miso actual data from SESCO database
# miso_verification <- query_actual_wind_mw(start = ymd('2018-02-11'), end = max(windmiso_actuals$date, na.rm = TRUE), region_id = 67529, timeZone = "US/Central") %>%
#   mutate(date = floor_date(date, "minute"))
# #use this line during day light savings time
# #mutate(date = date - hours(1) - seconds(2))

# colnames(miso_verification)[colnames(miso_verification) == "POWERMW"] <- "Actuals"
# #colnames(miso_verification)[colnames(miso_verification) == "RTO_NAME"] <- "Iso"
# miso_actuals <- miso_verification %>%
#   select(date,Actuals)
# 
# #Join Miso actuals to the forecasted data
# miso_all <- left_join(windmiso_actuals, miso_actuals) %>%
#   group_by(date) %>%
#   gather(type,mw,-date)
# #----------------------------------------------------
# #Miso Data manipulation
# colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"

#----------------------------------------------------

# #Load SPP data
# Wind_forecasts <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
#                              range = cell_cols("A:E") , col_names = TRUE)
# 
# #Wind_forecasts$date <- as.POSIXct(Wind_forecasts$date, format = "%Y-%m-%d %H:%M:%S")
# 
# Wind_forecasts <- Wind_forecasts %>%
#   mutate(date = ymd_hms(date, tz = "US/Central")) %>%
#   mutate(date = date - hours(6))
# 
# #SPP Actuals pull------------------------------------------------------
# # Get SPP actuals (verification)
# 
# #I dont think this code has the correct data for SPP but the other SPP code shows no data
# SPP_verification <- query_actual_wind_mw(start = ymd('2018-02-11') , end = max(Wind_forecasts$date, na.rm = TRUE), region_id = 68, timeZone = "US/Central") %>% 
#   mutate(date = date)
# #use the following line during day light savings time
# #mutate(date = date - hours(1))
# 
# colnames(SPP_verification)[colnames(SPP_verification) == "POWERMW"] <- "Actuals"
# SPP_actuals <- SPP_verification %>%
#   filter(., grepl(":00:00",date)) %>%
#   select(date, Actuals)
# #SPP_actuals <- filter(SPP_actuals, grepl(":00:00", date))
# 
# 
# #Join tables -------------------------------------------------------------
# #Join the data together
# SPP_all <- left_join(Wind_forecasts, SPP_actuals) %>%
#   group_by(date) %>%
#   gather(type,mw,-date)
# #filter(date>(start) & date<end)
# #-------------------------------------------------------------------------------------
# #SPP data manipulation
# colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"




#Load ERCOT Data

#Load Next Day files in
# E_nxtdaywnd <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
#                           range = cell_cols("A:C") , col_names = TRUE)
# 
# #E_nxtdaywnd$date <- as.POSIXct(E_nxtdaywnd$date, format = "%Y-%m-%d %H:%M:%S")
# E_nxtdaywnd <- E_nxtdaywnd %>%
#   mutate(date = ymd_hms(date, tz = "US/Central")) %>%
#   mutate(date = date - hours(6))
# 
# # Ercot Actuals Pull ------------------------------------------------------
# # Get ercot actuals (verification)
# ercot_actuals <- query_wind_mw_ercot_actuals(start = ymd("2018-3-12") , end = max(E_nxtdaywnd$date, na.rm = TRUE) , hourly = "yes", return_sql = F) %>% 
#   select(-c(REGIONORZONEID)) %>%
#   mutate(date = date + hours(1))
# 
# colnames(ercot_actuals)[colnames(ercot_actuals) == "MW"] <- "Actuals"
# 
# 
# # 3 Tier Data Pull --------------------------------------------------------
# # Gets 3 tier's ERCOT forecast. The data comes in individual farms, so must sum it all to get Forecast for ERCOT total 
# 
# regions <- find_region() %>% filter(CAID==9) 
# 
# farms <- find_wind_farms(center_longitude = -103.06708, center_latitude = 31.27669, max_distance_from_center = 1000) %>% 
#   filter(REGIONID %in% regions$REGIONID) %>% 
#   filter(OBSOLETEDAT == "9999-12-31 00:00:00")
# 
# timeZone <- "US/Central"
# 
# data3tier <- query_wind_DA_fcast_3tier_by_region(start = ymd('2018-02-11'), end = today(), 67509) %>% 
#   select(-c(SAMPLETIMESTAMP,POWERMW24)) %>% 
#   gather(hour,mw,-c(REGIONID,MARKETDAY,FARMID,FARMNAME))%>% 
#   mutate(hour = as.numeric(gsub("POWERMW","", hour)), date = ymd_h(paste(MARKETDAY, hour, sep = ":"), tz = timeZone)) %>% 
#   select(-c(REGIONID,FARMID,MARKETDAY))
# 
# data3tier_sum <- data3tier %>% 
#   group_by(date) %>% 
#   summarize(`3tier` = sum(mw)) %>% 
#   filter(date>ymd('2018-02-11')) %>% 
#   filter(!is.na(`3tier`)) 
# 
# # Join tables -------------------------------------------------------------
# # Join the data together
# ercot_all <- data3tier_sum %>% 
#   group_by(date) %>% 
#   left_join(.,E_nxtdaywnd) %>% 
#   left_join(., ercot_actuals) %>% 
#   gather(type,mw,-date)
# #filter(date>(start+1) & date<end)


#----------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    updateDateRangeInput(session, "dateRange", start = as.Date(input$client_time) - days(1), end = as.Date(input$client_time), max = as.Date(input$client_time))
  })
  
  #MISO reactive data retrieval from master CSV and check to see if the file is up to date------------------------------------------------------
  #read in latest csvs
  latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                            range = cell_cols("A:E") , col_names = TRUE)
  
  windmiso_actuals <- reactive({
    #check for max date
    max_date = as.Date(format(max(latest_miso$date, na.rm = TRUE), "%Y-%m-%d"))
    
    if(max_date != as.Date(input$client_time) + days(2)) {
      output <- list()
      
      repeat{
        min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
        #add in code that pulls in the file with the string names by min_add and pull that data out similar to the for loop for the giant data pull
        miso_wind <- list.files("Z://MISO_wind/Miso/", pattern = min_add) %>%
          data.frame(files = .) %>%
          filter(grepl(".xlsx", files)) %>%
          pull(.)
        
        date <- paste(substr(miso_wind, 19, 20),substr(miso_wind, 15, 16), sep = "-")
        date <- paste(date, substr(miso_wind, 17, 18), sep = "-")
        date <- paste0("20", date)
        x <- read_excel(sprintf("Z://MISO_wind/Miso/%s", miso_wind), sheet = 2, 
                        range = "B2:J26" , col_names = TRUE) %>%
          mutate(date = date)
        output[[paste(gsub(".rds" , replacement = "", miso_wind))]] <- x
        
        max_date = max_date + days(1)
        
        if(max_date == as.Date(input$client_time) + days(2)){
          break
        }
      }
      
      timezones <- data.frame(tz = OlsonNames())
      windmiso_two <- do.call(rbind, output) %>%
        mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
        select(date, Sesco, '3 Tier', MDA, WSI) %>%
        na.omit()
      
      #rbind those csvs to master csv
      windmiso_new <- rbind(latest_miso, windmiso_two) %>%
        group_by(date)
      
      #save master csv
      write_xlsx(windmiso_new, path = "Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx",
                 col_names = TRUE)#, row.names = FALSE, append = FALSE)
      
      # Load Miso data from master excel file
      latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                                range = cell_cols("A:E") , col_names = TRUE)
      
      #changing the time from UTC to US/Central
      windmiso_actuals <- latest_miso %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(6))
      
      colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"
      
      windmiso_actuals <- windmiso_actuals
      
    }else{
      #else take in same file and no modifications needed
      # Load Miso data from master excel file
      latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                                range = cell_cols("A:E") , col_names = TRUE)
      
      #Changing the time from UTC to US/Central
      windmiso_actuals <- latest_miso %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(6))
      
      colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"
      
      windmiso_actuals <- windmiso_actuals
      
    }
  })
  
  #MISO verification data retrieval
  #Load Miso actual data from SESCO database
  miso_verification <- reactive({
  miso_verification <- query_actual_wind_mw(start = ymd('2018-02-11'), end = as.Date(input$client_time), region_id = 67529, timeZone = "US/Central") %>%
    mutate(date = floor_date(date, "minute"))
  
  colnames(miso_verification)[colnames(miso_verification) == "POWERMW"] <- "Actuals"
  
  miso_verification <- miso_verification
  })
  #use this line during day light savings time
  #mutate(date = date - hours(1) - seconds(2))
  
  miso_actuals <- reactive({
    miso_verification() %>%
    select(date,Actuals)
  })
    
  #Join Miso actuals to the forecasted data
  miso_all <- reactive({
    left_join(windmiso_actuals(), miso_actuals()) %>%
    group_by(date) %>%
    gather(type,mw,-date)
  })
  #------------------------------------------------------------------------------------------------
  
  #MISO reactive data -----------------------------------------------------------------------------  
  selected_data_miso <- reactive({
    req(input$dateRange)
    validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
    miso_all()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_data_misoview <- reactive({
    miso_actuals()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  
  
  selected_windmiso_actuals <- reactive({
    windmiso_actuals()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  miso_actuals_table <- reactive({
    miso_actuals()%>%
      mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
      filter(date >= as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2]))
  })
  
  #SPP reactive data retrieval from master CSV and check to see if the file is up to date-------------------------------------------------------------
  
  latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:E") , col_names = TRUE)
  Wind_forecasts <- reactive({
    #check for max date
    max_date = as.Date(format(max(latest_spp$date, na.rm = TRUE), "%Y-%m-%d"))
    
    if(max_date != as.Date(input$client_time) + days(2)) {
      output <- list()
      
      repeat{
        min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
        #add in code that pulls in the file with the string names by min_add and pull that data out simiar to the for loop for the giant data pull
        spp_wind <- list.files("Z://SPP_Wind/SPP/", pattern = min_add) %>%
          data.frame(files = .) %>%
          filter(grepl(".xlsx", files)) %>%
          pull(.)
        
        date <- paste(substr(spp_wind, 18, 19),substr(spp_wind, 14, 15), sep = "-")
        date <- paste(date, substr(spp_wind, 16, 17), sep = "-")
        date <- paste0("20", date)
        x <- read_excel(sprintf("Z://SPP_Wind/SPP/%s", spp_wind), sheet = 2,
                        range = "B2:K26" , col_names = TRUE) %>%
          mutate(date = date)
        output[[paste(gsub(".rds" , replacement = "", spp_wind))]] <- x
        
        max_date = max_date + days(1)
        
        if(max_date == as.Date(input$client_time) + days(2)){
          break
        }
      }
      
      timezones <- data.frame(tz = OlsonNames())
      windspp_two <- do.call(rbind, output) %>%
        mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
        select(date, Sesco, '3 Tier', WSI, SPP) %>%
        na.omit()
      
      #rbind those csvs to master csv
      windspp_new <- rbind(latest_spp, windspp_two) %>%
        group_by(date)
      
      #save master csv
      write_xlsx(windspp_new, path = "Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx",
                 col_names = TRUE)#, row.names = FALSE, append = FALSE)
      
      latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:E") , col_names = TRUE)
      
      Wind_forecasts <- latest_spp %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(6))
      
      colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"
      
      Wind_forecasts <- Wind_forecasts
      
    }else{
      latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:E") , col_names = TRUE)
      
      Wind_forecasts <- latest_spp %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(6))
      
      colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"
      
      Wind_forecasts <- Wind_forecasts
    }
  })
  
  #I dont think this code has the correct data for SPP but the other SPP code shows no data
  SPP_verification <- reactive({
    SPP_verification <- query_actual_wind_mw(start = ymd('2018-02-11') , end = as.Date(input$client_time), region_id = 68, timeZone = "US/Central") %>% 
      mutate(date = date)
  
    colnames(SPP_verification)[colnames(SPP_verification) == "POWERMW"] <- "Actuals"
  
    SPP_verification <- SPP_verification
  })
  #use the following line during day light savings time
  #mutate(date = date - hours(1))
  
  SPP_actuals <- reactive({
    SPP_actuals <- SPP_verification() %>%
      filter(., grepl(":00:00",date)) %>%
      select(date, Actuals)
  })
  
  SPP_all <- reactive({
  SPP_all <- left_join(Wind_forecasts(), SPP_actuals()) %>%
    group_by(date) %>%
    gather(type,mw,-date)
  })
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  
  #SPP reactive data ----------------------------------------------------------------------
  selected_data_spp <- reactive({
    req(input$dateRange)
    validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
    SPP_all()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_data_sppview <- reactive({
    SPP_actuals() %>% 
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_wind_forecasts <- reactive({
    Wind_forecasts()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  SPP_actuals_table <- reactive({
    SPP_actuals()%>%
      mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
      filter(date >= as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2]))
  })
  
  #ERCOT reactive data retrieval from master CSV and check to see if the file is up to date-------------------------------------------------------------
  latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                            range = cell_cols("A:C") , col_names = TRUE)
  
  E_nxtdaywnd <- reactive({
  max_date = as.Date(format(max(latest_ercot$date, na.rm = TRUE), "%Y-%m-%d"))
  #max_date = max_date - days(1)
  #read in all daily csvs max date and greater
  # if (max_date != today() +day(2)) {
  #   #min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
  # }
  #implement do while- like statement in order to check max date and continue to make a string for each time there needs to be a day added
  #min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
  
  # miso_wind <- list.files("/MISO_wind/Miso/", pattern = min_add) %>%
  #   data.frame(files = .) %>%
  #   filter(grepl(".xlsx", files)) %>%
  #   pull(.)
  #
  if(max_date != as.Date(input$client_time) + days(2)) {
    output <- list()
    
    repeat{
      min_add = paste(substr(max_date,1,10))
      #add in code that pulls in the file with the string names by min_add and pull that data out simiar to the for loop for the giant data pull
      ercot_wind <- list.files("Z://ERCOT_Wind/Next Day/", pattern = min_add) %>%
        data.frame(files = .) %>%
        filter(grepl(".xlsx", files)) %>%
        pull(.)
      
      date <- paste0(substr(ercot_wind, 12, 21))
      x <- read_excel(sprintf("Z://ERCOT_Wind/Next Day/%s", ercot_wind), sheet = 1,
                      range = "A1:D25" , col_names = TRUE) %>%
        mutate(date = date)
      output[[paste(gsub(".rds" , replacement = "", ercot_wind))]] <- x
      
      max_date = max_date + days(1)
      
      if(max_date == as.Date(input$client_time) + days(2)){
        break
      }
    }
    
    timezones <- data.frame(tz = OlsonNames())
    windercot_two <- do.call(rbind, output) %>%
      mutate(date = ymd_h(paste0(date,":",He),tz = "US/Central") ) %>%
      select(date, ERCOT, SESCO) %>%
      na.omit()
    
    #rbind those csvs to master csv
    windercot_new <- rbind(latest_ercot, windercot_two) %>%
      group_by(date)
    
    #save master csv
    write_xlsx(windercot_new, path = "Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx",
               col_names = TRUE)#, row.names = FALSE, append = FALSE)
    
    latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                               range = cell_cols("A:C") , col_names = TRUE)
    
    E_nxtdaywnd <- latest_ercot %>%
      mutate(date = ymd_hms(date, tz = "US/Central")) %>%
      mutate(date = date - hours(6))
    
    E_nxtdaywnd <- E_nxtdaywnd
    
  }else{
    latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                               range = cell_cols("A:C") , col_names = TRUE)
    
    E_nxtdaywnd <- latest_ercot %>%
      mutate(date = ymd_hms(date, tz = "US/Central")) %>%
      mutate(date = date - hours(6))
    
    E_nxtdaywnd <- E_nxtdaywnd
  }
  })
  
  # Ercot Actuals Pull ------------------------------------------------------
  # Get ercot actuals (verification)
  ercot_actuals <- reactive({
  ercot_actuals <- query_wind_mw_ercot_actuals(start = ymd("2018-3-12") , end = as.Date(input$client_time) , hourly = "yes", return_sql = F) %>% 
    select(-c(REGIONORZONEID)) %>%
    mutate(date = date + hours(1))
  
  colnames(ercot_actuals)[colnames(ercot_actuals) == "MW"] <- "Actuals"
  
  ercot_actuals <- ercot_actuals
  })
  
  # 3 Tier Data Pull --------------------------------------------------------
  # Gets 3 tier's ERCOT forecast. The data comes in individual farms, so must sum it all to get Forecast for ERCOT total 
  data3tier_sum <- reactive({
  regions <- find_region() %>% filter(CAID==9) 
  
  farms <- find_wind_farms(center_longitude = -103.06708, center_latitude = 31.27669, max_distance_from_center = 1000) %>% 
    filter(REGIONID %in% regions$REGIONID) %>% 
    filter(OBSOLETEDAT == "9999-12-31 00:00:00")
  
  timeZone <- "US/Central"
  
  data3tier <- query_wind_DA_fcast_3tier_by_region(start = ymd('2018-02-11'), end = as.Date(input$client_time), 67509) %>% 
    select(-c(SAMPLETIMESTAMP,POWERMW24)) %>% 
    gather(hour,mw,-c(REGIONID,MARKETDAY,FARMID,FARMNAME))%>% 
    mutate(hour = as.numeric(gsub("POWERMW","", hour)), date = ymd_h(paste(MARKETDAY, hour, sep = ":"), tz = timeZone)) %>% 
    select(-c(REGIONID,FARMID,MARKETDAY))
  
  data3tier_sum <- data3tier %>% 
    group_by(date) %>% 
    summarize(`3tier` = sum(mw)) %>% 
    filter(date>ymd('2018-02-11')) %>% 
    filter(!is.na(`3tier`))
  })
  
  # Join the data together
  ercot_all <- reactive({
  ercot_all <- data3tier_sum() %>% 
    group_by(date) %>% 
    left_join(.,E_nxtdaywnd()) %>% 
    left_join(., ercot_actuals()) %>% 
    gather(type,mw,-date)
  })
  #---------------------------------------------------------------------------------------------------------------------------------------------------
  
  #ERCOT reactive data calls---------------------------------------------------------------  
  #date > as.POSIXct(input$dateRange[1]) & date < as.POSIXct(input$dateRange[2])
  selected_data_ercot <-reactive({
    req(input$dateRange)
    validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Please provide both a start and an end date."))
    validate(need(input$dateRange[1] < input$dateRange[2], "Error: Start date should be earlier than end date."))
    ercot_all()%>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_ercot_actuals <- reactive({
    ercot_actuals() %>% 
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_E_nxtdaywnd <- reactive({
    E_nxtdaywnd() %>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  selected_data3tier_sum <- reactive({
    data3tier_sum() %>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])
  })
  ercot_actuals_table <- reactive({
    ercot_actuals()%>%
      mutate_at(vars("Actuals"), funs(round(., digits = 1))) %>%
      filter(date(date) >= input$dateRange[1] & date(date) < input$dateRange[2])%>%
      mutate(date = date - hours(6))
  })
  
  
  #Output for Miso tab ---------------------------------------------------------------------
  output$MISO <- renderPlot({
    
    ggplot(selected_data_miso(), aes(date,mw,col=type)) + geom_line(size=1) + ggtitle("Forecast Vs. Actual")
    
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
  # output$viewmiso <- DT::renderDataTable({
  #   DT::datatable(miso_actuals_table())
  # })
  
  
  #Output for SPP tab -------------------------------------------------------------------------------
  output$SPP <- renderPlot({
    
    ggplot(selected_data_spp(), aes(date,mw,col=type)) + geom_line(size=1) + ggtitle("Forecast Vs. Actual")
    
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
  # output$viewspp <- DT::renderDataTable({
  #   DT::datatable(SPP_actuals_table())
  # })
  
  
  #Output for ercot tab ---------------------------------------------------------------------------
  output$ERCOT <- renderPlot({
    
    ggplot(selected_data_ercot(), aes(date,mw,col=type)) + geom_line(size=1) + ggtitle("Forecast Vs. Actual")
    
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
  
  # output$viewercot <- DT::renderDataTable({
  #   DT::datatable(ercot_actuals_table())
  # })
  
}
