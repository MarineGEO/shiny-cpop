# shiny app demo showing SERC water quality, level and met data from early 2017
library(shiny)
#library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)

# load the data file when application starts
#wq_data <- read_csv("data/2017_Water_Quality_RAW_SERC.csv") 
wq_data <- read_csv(url("https://dl.dropboxusercontent.com/s/n8sagrl8iwdmktm/2017_Water_Quality_RAW_SERC.csv?dl=0")) 
names(wq_data) <- make.names(names(wq_data))

#we_data <- read_csv("data/2017_MET_RAW_SERC.csv")
we_data <- read_csv(url("https://dl.dropboxusercontent.com/s/rnkq0i0r7uxc5l5/2017_MET_RAW_SERC.csv?dl=0"))
names(we_data) <- make.names(names(we_data))

#wl_data <- read_csv("data/2017_Water_Level_RAW_SERC.csv")
wl_data <- read_csv(url("https://dl.dropboxusercontent.com/s/byukiwxjtxkw0nm/2017_Water_Level_RAW_SERC.csv?dl=0"))
names(wl_data) <- make.names(names(wl_data))


# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("DEMO: SERC MarineGEO Sensors"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Dropdown Selector for Sensor Dataset
      selectInput("sensor", "Sensor:",
                  c("Water Quality" = "wq",
                    "Weather" = "we",
                    "Water Level" = "wl"), selected = "wl"),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # conditional inputs
       uiOutput('variable'),
     
      # br() element to introduce extra vertical spacing ----
      br(),
       
      selectInput("timeSpan", "View data from previous:",
                  c("3 hours" = 3,
                    "6 hours" = 6,
                    "12 hours" = 12,
                    "1 day" = 24,
                    "2 days" = 2*24,
                    "3 days" = 3*24,
                    "4 days" = 4*24,
                    "5 days" = 5*24,
                    "1 Week" = 7*24,
                    "10 days" = 10*24,
                    "2 Weeks" = 2*7*24,
                    "3 Weeks" = 3*7*24,
                    "1 Month" = 31*24,
                    "6 Weeks" = 6*7*24,
                    "2 Months" = 31*2*24), selected = 1)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic for app ----
server <- function(input, output) {

  # function to return dataset for a selected sensor type
  sensorDataSource <- reactive({
    cat(file=stderr(), "Switching to", input$sensor, "\n")
    d <- switch(input$sensor,"wq" = wq_data,"we" = we_data, "wl"= wl_data)
    return(d)
  })
  
  # builds ui dropdown selctor from the data source column names
  output$variable = renderUI({
      mydata <- sensorDataSource()
      ignore_cols <- c("Timestamp", "Site") # specify columns to ignore
      colnames <- names(mydata)[!names(mydata) %in% ignore_cols]
      selectInput('selectedVariable', 'Variable', colnames, selected=colnames[0])
    })
  
  # Generate a plot of the data ----
  output$plot <- renderPlot({
      #(file=stderr(), "Ploting", input$sensor, " - ", input$selectedVariable, "\n")

      sensorDataSource() %>% filter(Timestamp>ymd_hms(max(Timestamp))-hours(input$timeSpan)) %>% 
        ggplot(aes_string(x="Timestamp", y=input$selectedVariable))+
        geom_point()+
        theme_bw()

  })
  
  
  # Generate an HTML table view of the data ----
  output$table <- DT::renderDataTable(DT::datatable({
    
    d <- sensorDataSource() %>% filter(Timestamp>ymd_hms(max(Timestamp))-hours(input$timeSpan))
    d
  }))
  
}

# Create Shiny app ----
shinyApp(ui, server)