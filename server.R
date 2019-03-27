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

#----------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  observe({
    updateDateRangeInput(session, "dateRange", start = as.Date(input$client_time) - days(1), end = as.Date(input$client_time), max = as.Date(input$client_time))
  })
  
  #MISO reactive data retrieval from master CSV and check to see if the file is up to date------------------------------------------------------
  #read in latest csvs
  
    latest_miso <- read_excel("/data/rdata/rdatashare/weather/MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
    # latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                            range = cell_cols("A:G") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric","numeric"))
  
  windmiso_actuals <- reactive({
    #check for max date
    max_date = as.Date(format(max(latest_miso$date, na.rm = TRUE), "%Y-%m-%d"))
    
    if(max_date < as.Date(input$client_time) + days(1)) {
      output <- list()
      
      repeat{
        min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
        #add in code that pulls in the file with the string names by min_add and pull that data out similar to the for loop for the giant data pull
        miso_wind <- list.files("/data/rdata/rdatashare/weather/MISO_wind/Miso/", pattern = min_add) %>%
        # miso_wind <- list.files("Z://MISO_wind/Miso/", pattern = min_add) %>%
          data.frame(files = .) %>%
          filter(grepl(".xlsx", files)) %>%
          pull(.)
        
        date <- paste(substr(miso_wind, 19, 20),substr(miso_wind, 15, 16), sep = "-")
        date <- paste(date, substr(miso_wind, 17, 18), sep = "-")
        date <- paste0("20", date)
        x <- read_excel(sprintf("/data/rdata/rdatashare/weather/MISO_wind/Miso/%s", miso_wind), sheet = 2,
        # x <- read_excel(sprintf("Z://MISO_wind/Miso/%s", miso_wind), sheet = 2,
                        range = "B2:O26" , col_names = TRUE) %>%
          mutate(date = date)
        output[[paste(gsub(".rds" , replacement = "", miso_wind))]] <- x
        
        max_date = max_date + days(1)
        
        if(max_date >= as.Date(input$client_time) + days(1)){
          break
        }
      }
      
      timezones <- data.frame(tz = OlsonNames())
      windmiso_two <- do.call(rbind, output) %>%
        mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
        select(date, Sesco, '3 Tier', MDA, WSI, MISO, Risk)
      
      #rbind those csvs to master csv
      windmiso_new <- rbind(latest_miso, windmiso_two) %>%
        group_by(date)
      
      #save master csv
      # write_xlsx(windmiso_new, path = "Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx",
      write_xlsx(windmiso_new, path = "/data/rdata/rdatashare/weather/MISO_wind/Miso/Master excel sheet/Master_miso.xlsx",
                 col_names = TRUE)#, row.names = FALSE, append = FALSE)
      
      # Load Miso data from master excel file
      # latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
      latest_miso <- read_excel("/data/rdata/rdatashare/weather/MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                                range = cell_cols("A:G") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric","numeric"))
      
      #changing the time from UTC to US/Central
      windmiso_actuals <- latest_miso %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(5)) #use during daylight savings time 
        #mutate(date = date - hours(6)) #use otherwise
      
      colnames(windmiso_actuals)[colnames(windmiso_actuals) == "3 Tier"] <- "vaisala"
      
      windmiso_actuals <- windmiso_actuals
      
    }else{
      #else take in same file and no modifications needed
      # Load Miso data from master excel file
      # latest_miso <- read_excel("Z://MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
      latest_miso <- read_excel("/data/rdata/rdatashare/weather/MISO_wind/Miso/Master excel sheet/Master_miso.xlsx", sheet = 1,
                                range = cell_cols("A:G") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric","numeric"))
      
      #Changing the time from UTC to US/Central
      windmiso_actuals <- latest_miso %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(5))#use during daylight savings time 
      #mutate(date = date - hours(6)) #use otherwise
      
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
    #group_by(date) %>%
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
  
  # latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
  latest_spp <- read_excel("/data/rdata/rdatashare/weather/SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:F") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric"))
  Wind_forecasts <- reactive({
    #check for max date
    max_date = as.Date(format(max(latest_spp$date, na.rm = TRUE), "%Y-%m-%d"))
    
    if(max_date < as.Date(input$client_time) + days(1)) {
      output <- list()
      
      repeat{
        min_add = paste(substr(max_date,6,7),substr(max_date,9,10),substr(max_date,3,4), sep = "")
        #add in code that pulls in the file with the string names by min_add and pull that data out simiar to the for loop for the giant data pull
        spp_wind <- list.files("/data/rdata/rdatashare/weather/SPP_Wind/SPP/", pattern = min_add) %>%
        # spp_wind <- list.files("Z://SPP_Wind/SPP/", pattern = min_add) %>%
          data.frame(files = .) %>%
          filter(grepl(".xlsx", files)) %>%
          pull(.)
        
        date <- paste(substr(spp_wind, 18, 19),substr(spp_wind, 14, 15), sep = "-")
        date <- paste(date, substr(spp_wind, 16, 17), sep = "-")
        date <- paste0("20", date)
        x <- read_excel(sprintf("/data/rdata/rdatashare/weather/SPP_Wind/SPP/%s", spp_wind), sheet = 2,
        # x <- read_excel(sprintf("Z://SPP_Wind/SPP/%s", spp_wind), sheet = 2,
                        range = "B2:O26" , col_names = TRUE) %>%
          mutate(date = date)
        output[[paste(gsub(".rds" , replacement = "", spp_wind))]] <- x
        
        max_date = max_date + days(1)
        
        if(max_date >= as.Date(input$client_time) + days(1)){
          break
        }
      }
      
      timezones <- data.frame(tz = OlsonNames())
      windspp_two <- do.call(rbind, output) %>%
        mutate(date = ymd_h(paste0(date,":",LDT),tz = "US/Central") ) %>%
        select(date, Sesco, '3 Tier', WSI, SPP, Risk) %>%
        na.omit()
      
      #rbind those csvs to master csv
      windspp_new <- rbind(latest_spp, windspp_two) %>%
        group_by(date)
      
      #save master csv
      # write_xlsx(windspp_new, path = "Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx",
      write_xlsx(windspp_new, path = "/data/rdata/rdatashare/weather/SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx",
                 col_names = TRUE)#, row.names = FALSE, append = FALSE)
      
      # latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
      latest_spp <- read_excel("/data/rdata/rdatashare/weather/SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:F") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric"))
      
      Wind_forecasts <- latest_spp %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(5))#use during daylight savings time 
      #mutate(date = date - hours(6)) #use otherwise
      
      colnames(Wind_forecasts)[colnames(Wind_forecasts) == "3 Tier"] <- "vaisala"
      
      Wind_forecasts <- Wind_forecasts
      
    }else{
      # latest_spp <- read_excel("Z://SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
      latest_spp <- read_excel("/data/rdata/rdatashare/weather/SPP_Wind/SPP/Master excel sheet/Master_spp.xlsx", sheet = 1,
                               range = cell_cols("A:F") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric","numeric","numeric"))
      
      Wind_forecasts <- latest_spp %>%
        mutate(date = ymd_hms(date, tz = "US/Central")) %>%
        mutate(date = date - hours(5))#use during daylight savings time 
      #mutate(date = date - hours(6)) #use otherwise
      
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
  # latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
  latest_ercot <- read_excel("/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                            range = cell_cols("A:D") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric"))
  
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
  if(max_date < as.Date(input$client_time) + days(1)) {
    output <- list()
    
    repeat{
      min_add = paste(substr(max_date,1,10))
      #add in code that pulls in the file with the string names by min_add and pull that data out simiar to the for loop for the giant data pull
      ercot_wind <- list.files("/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/", pattern = min_add) %>%
      # ercot_wind <- list.files("Z://ERCOT_Wind/Next Day/", pattern = min_add) %>%
        data.frame(files = .) %>%
        filter(grepl(".xlsx", files)) %>%
        pull(.)
      
      date <- paste0(substr(ercot_wind, 12, 21))
      # x <- read_excel(sprintf("Z://ERCOT_Wind/Next Day/%s", ercot_wind), sheet = 1,
      x <- read_excel(sprintf("/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/%s", ercot_wind), sheet = 1,
                      range = "A1:H25" , col_names = TRUE) %>%
        mutate(date = date)
      output[[paste(gsub(".rds" , replacement = "", ercot_wind))]] <- x
      
      max_date = max_date + days(1)
      
      if(max_date >= as.Date(input$client_time) + days(1)){
        break
      }
    }
    
    timezones <- data.frame(tz = OlsonNames())
    windercot_two <- do.call(rbind, output) %>%
      mutate(date = ymd_h(paste0(date,":",He),tz = "US/Central") ) %>%
      select(date, ERCOT, SESCO, Risk) %>%
      na.omit()
    
    #rbind those csvs to master csv
    windercot_new <- rbind(latest_ercot, windercot_two) %>%
      group_by(date)
    
    #save master csv
    # write_xlsx(windercot_new, path = "Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx",
    write_xlsx(windercot_new, path = "/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx",
               col_names = TRUE)#, row.names = FALSE, append = FALSE)
    
    # latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
    latest_ercot <- read_excel("/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                               range = cell_cols("A:D") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric"))
    
    E_nxtdaywnd <- latest_ercot %>%
      mutate(date = ymd_hms(date, tz = "US/Central")) %>%
      mutate(date = date - hours(5))#use during daylight savings time 
    #mutate(date = date - hours(6)) #use otherwise
    
    E_nxtdaywnd <- E_nxtdaywnd
    
  }else{
    # latest_ercot <- read_excel("Z://ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
    latest_ercot <- read_excel("/data/rdata/rdatashare/weather/ERCOT_Wind/Next Day/Master excel sheet/Master_ercot.xlsx", sheet = 1,
                               range = cell_cols("A:D") , col_names = TRUE, col_types = c("date","numeric", "numeric", "numeric"))
    
    E_nxtdaywnd <- latest_ercot %>%
      mutate(date = ymd_hms(date, tz = "US/Central")) %>%
      mutate(date = date - hours(5))#use during daylight savings time 
    #mutate(date = date - hours(6)) #use otherwise
    
    E_nxtdaywnd <- E_nxtdaywnd
  }
  })
  
  # Ercot Actuals Pull ------------------------------------------------------
  # Get ercot actuals (verification)
  # GOOD ON THE SERVER BUT NOT LOCALLY TESTING
  ercot_actuals <- reactive({
  ercot_actuals <- query_wind_mw_ercot_actuals(start = ymd("2018-3-12") , end = as.Date(input$client_time) , hourly = "yes", return_sql = F) %>% #as.Date(input$client_time) ymd("2019-03-14")
    select(-c(REGIONORZONEID)) %>%
    mutate(date = date + hours(1))

  colnames(ercot_actuals)[colnames(ercot_actuals) == "MW"] <- "Actuals"

  ercot_actuals <- ercot_actuals
  
  ##### USE THE FOLLOWING CODE ONLY WHEN TESTING LOCALLY #####
  # ercot_actuals_one <- query_wind_mw_ercot_actuals(start = ymd("2018-3-12") , end = ymd("2019-3-09") , hourly = "yes", return_sql = F) %>% #as.Date(input$client_time) ymd("2019-03-14")
  #   select(-c(REGIONORZONEID)) %>%
  #   mutate(date = date + hours(1))
  # 
  # colnames(ercot_actuals_one)[colnames(ercot_actuals_one) == "MW"] <- "Actuals"
  # 
  # ercot_actuals_two <- query_wind_mw_ercot_actuals(start = ymd("2019-3-11") , end = as.Date(input$client_time) , hourly = "yes", return_sql = F) %>% #as.Date(input$client_time) ymd("2019-03-14")
  #   select(-c(REGIONORZONEID)) %>%
  #   mutate(date = date + hours(1))
  # 
  # colnames(ercot_actuals_two)[colnames(ercot_actuals_two) == "MW"] <- "Actuals"
  # 
  # ercot_actuals <- full_join(ercot_actuals_one,ercot_actuals_two)
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
             WSI = abs(Actuals - WSI),
             MISO = abs(Actuals - MISO),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, Sesco, '3 Tier', WSI, MDA, MISO, Risk) %>% 
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
             WSI = abs(Actuals - WSI),
             MISO = abs(Actuals - MISO),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, Sesco, '3 Tier', WSI, MDA, MISO, Risk) %>% 
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
             SPP = abs(Actuals - SPP),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, Sesco, '3 Tier', WSI, SPP, Risk) %>% 
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
             SPP = abs(Actuals - SPP),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, Sesco, '3 Tier', WSI, SPP, Risk) %>% 
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
             ERCOT = abs(Actuals - ERCOT),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, '3tier', SESCO, ERCOT, Risk) %>% 
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
             ERCOT = abs(Actuals - ERCOT),
             Risk = abs(Actuals - Risk)) %>%  
      select(date, '3tier', SESCO, ERCOT, Risk) %>% 
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
  
  # creates a data table with only actual data 
  # output$viewercot <- DT::renderDataTable({
  #   DT::datatable(ercot_actuals_table())
  # })
  
}
