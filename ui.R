library(lubridate)
library(shiny)
library(shinycssloaders)
library(DT)

ui <- navbarPage(
  "Wind Verification",
      fluidPage(
        
        HTML('<input type="text" id="client_time" name="client_time" style="display: none;" > '),
        tags$script('
                                          $(function() {
                                          var time_now = new Date()
                                          var month_now=time_now.getMonth()+1
                                          $("input#client_time").val(time_now.getFullYear()+"-"+month_now+"-"+time_now.getDate())
                                          });
                                          '),
        
             sidebarPanel(width = 2,
                          dateRangeInput(inputId = "dateRange", label = 'Date Range:', min = ymd('2018-02-11')),
                          submitButton("Plot", icon("refresh"))
                          #selectInput('iso', "Choose ISO", choice = c("MISO", "SPP", "ERCOT"), 
                                      #selected = "MISO", multiple = F)
                          ),
             mainPanel(
                 tabsetPanel(type = "tabs",
                             tabPanel("ERCOT", plotOutput("ERCOT") %>% withSpinner(color="#0dc5c1"), 
                                      "Mean Absolute Error for selected time period", verbatimTextOutput("summaryercot"),
                                      plotOutput("ercotmae")),#, #DT::dataTableOutput("viewercot") %>% 
                                        #withSpinner(color="#0dc5c1")),
                             tabPanel("MISO", plotOutput("MISO") %>% withSpinner(color="#0dc5c1"), 
                                      "Mean Absolute Error for selected time period",verbatimTextOutput("summarymiso"),  
                                      plotOutput("misomae")),#, #DT::dataTableOutput("viewmiso") %>% 
                                        #withSpinner(color="#0dc5c1")),
                             tabPanel("SPP", plotOutput("SPP") %>% withSpinner(color="#0dc5c1"), 
                                      "Mean Absolute Error for selected time period",verbatimTextOutput("summaryspp"), 
                                      plotOutput("sppmae")))#, #DT::dataTableOutput("viewspp")%>% 
                                        #withSpinner(color="#0dc5c1")))
               )
             )
           )
