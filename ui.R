library(lubridate)
library(shiny)
library(shinycssloaders)
library(DT)

ui <- navbarPage(
  "Wind Verification",
      fluidPage(
        
             sidebarPanel(width = 2,
                          dateRangeInput(inputId = "dateRange", label = 'Date Range:', start = today() - days(1), 
                                         end = today(), min = ymd('2017-08-01'), max = today())
                          #actionButton("go", "Plot")
                          #selectInput('iso', "Choose ISO", choice = c("MISO", "SPP", "ERCOT"), 
                                      #selected = "MISO", multiple = F)
                          ),
             mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("ERCOT", plotOutput("ERCOT"), verbatimTextOutput("summaryercot"),
                                      plotOutput("ercotmae"), DT::dataTableOutput("viewercot") %>% 
                                        withSpinner(color="#0dc5c1")),
                             tabPanel("MISO", plotOutput("MISO"), verbatimTextOutput("summarymiso"),  
                                      plotOutput("misomae"), DT::dataTableOutput("viewmiso") %>% 
                                        withSpinner(color="#0dc5c1")),
                             tabPanel("SPP", plotOutput("SPP"), verbatimTextOutput("summaryspp"), 
                                      plotOutput("sppmae"), DT::dataTableOutput("viewspp")%>% 
                                        withSpinner(color="#0dc5c1")))
               )
             )
           )
