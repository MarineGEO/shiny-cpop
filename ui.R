library(shinydashboard)

# Source the sensor data list and the functions
source("functions.R", local=TRUE)
source("dataSource.R", local=TRUE)

header <- dashboardHeader(
  title = "MarineGEO Sensor Dashboard",
  titleWidth = 450
)

body <- dashboardBody(
  fluidRow(
    column(width = 3,
           box(width = NULL, status = "primary",
               selectInput("site", "Site:", c(siteList())), # Dropdown Selector for Sensor Dataset
               uiOutput('sensorSelectorUI'), # conditional dropdown for datasets at selected site
               uiOutput('parameterSelectorUI'), # conditional dropdown for sensor parameters at selected site and sensor
               #uiOutput('multiParameterSelectorUI'), # conditional dropdown for sensor parameters at selected site and sensor
               # time span selector
               selectInput("timeSpan", "View data from previous:",
                           c("3 hours" = 3, "6 hours" = 6, "12 hours" = 12, "1 day" = 24, "2 days" = 2*24, "3 days" = 3*24, 
                             "4 days" = 4*24, "5 days" = 5*24, "1 Week" = 7*24, "10 days" = 10*24, "2 Weeks" = 2*7*24, 
                             "3 Weeks" = 3*7*24, "1 Month" = 31*24, "6 Weeks" = 6*7*24, "2 Months" = 31*2*24), selected = 5*24)
           ),
           box(width = NULL, status = "primary",
               actionButton("downloadData", "Download Data") # Button to download the data for selected time span
           ),
           box(width = NULL, status = "primary",
                      htmlOutput("latestTime")
                  )

    ),
    column(width = 9,
               tabBox(width = NULL, 
                      tabPanel("Graph", plotOutput("singleParamPlot")),
                      tabPanel("Summary", tableOutput("summaryTable"))
                      )
            
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)