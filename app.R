# shiny app demo showing SERC water quality, level and met data from early 2017
library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
library(scales)

# Source the sensor data list and the functions
source("dataSource.R")
source("functions.R")

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("MarineGEO Sensors Dashboard"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("sensor", "Sensor:", c(sensorList())), # Dropdown Selector for Sensor Dataset
      
      br(),  # br() element to introduce extra vertical spacing ----
      
      uiOutput('variable'), # conditional inputs
      
      br(),  # br() element to introduce extra vertical spacing ----
       
      # time span selector
      selectInput("timeSpan", "View data from previous:",
                  c("3 hours" = 3, "6 hours" = 6, "12 hours" = 12, "1 day" = 24, "2 days" = 2*24, "3 days" = 3*24, 
                    "4 days" = 4*24, "5 days" = 5*24, "1 Week" = 7*24, "10 days" = 10*24, "2 Weeks" = 2*7*24, 
                    "3 Weeks" = 3*7*24, "1 Month" = 31*24, "6 Weeks" = 6*7*24, "2 Months" = 31*2*24), selected = 5*24),
      
      br(),# br() element to introduce extra vertical spacing ----
     
      downloadButton("downloadData", "Download"),  # Button to download the data for selected time span
      
      htmlOutput("latestValues")
      ),

    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Latest Reading", htmlOutput("latestTime"), tableOutput("table2")),
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Table", DT::dataTableOutput("table"))
      )
    )
  )
)

# Define server logic for app ----
server <- function(input, output) {

  # reactive function to return dataset for a selected sensor type
  sensorData <- reactive({
    cat(file=stderr(), "Switching to", input$sensor, "\n")
    d <- loadSensorCSV(sensorAttributeLookup(input$sensor, urlpath), sensorAttributeLookup(input$sensor, na))
    
    return(d)
  })
  
  # reactive function to filter dataset to time period (number hours) of interest
  sensorDataTimePeriod <- reactive({
    d <- sensorData() %>% filter(Timestamp>ymd_hms(max(Timestamp))-hours(input$timeSpan))
    return(d)
  })
  
  # reactive function to get the min/max dates of the filtered dataset
  TimePeriodMinMax <- reactive({
    minDate <- sensorDataTimePeriod() %>% pull(Timestamp) %>% min()
    maxDate <-sensorDataTimePeriod() %>% pull(Timestamp) %>% max()
    return(c(minDate, maxDate))
  })
  
  # builds ui dropdown selctor from the data source column names
  output$variable = renderUI({
      ignore_cols <- sensorAttributeLookup(input$sensor, ignore)[[1]] # grab the column names to ignore from the sensor table
      colnames <- names(sensorData())[!names(sensorData()) %in% ignore_cols] # select all the column names in csv that are not in ignore list
      
      namedLabels <- setNames(colnames, lapply(colnames, getLabel, labeledUnits=sensorAttributeLookup(input$sensor, units)))
      selectizeInput('selectedVariable', 'Variable', choices=namedLabels, selected=colnames[1], multiple=TRUE, options=(list(maxItems=3)))
    })
  
  # Generate a plot of the data ----
  output$plot <- renderPlot({
    Sys.sleep(1)
    if(length(input$selectedVariable)==1){
    
    sensorDataTimePeriod() %>% 
      ggplot(aes_string(x="Timestamp", y=input$selectedVariable, colour=input$selectedVariable))+
      geom_point(aes_string(x="Timestamp", y=input$selectedVariable))+
      ylab(getLabel(input$selectedVariable, sensorAttributeLookup(input$sensor, units)))+
      scale_colour_gradientn(colours = palette(c("black","dark blue","blue", "royalblue2", "skyblue3")))+
      scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")+
      theme_bw() + 
      theme(panel.border = element_blank(),
            panel.grid.major = element_blank(),
            plot.title = element_text(hjust = 0.5),
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=16),
            axis.text = element_text(size=14),
            legend.position="none", # position of legend or none
            legend.direction="horizontal", # orientation of legend
            legend.title= element_blank(), # no title for legend
            legend.key.size = unit(0.5, "cm"), # size of legend
            axis.line.x = element_line(color="black", size = 1),
            axis.line.y = element_line(color="black", size = 1))
    }
    else if (length(input$selectedVariable)>1){
      
      sensorDataTimePeriod() %>% 
        select(Timestamp, input$selectedVariable) %>% 
        tidyr::gather("Variable", "Value", -Timestamp) %>% 
        filter(Variable %in% input$selectedVariable) %>% 
        ggplot(aes(x=Timestamp, y=Value, colour=factor(Variable, labels=lapply(input$selectedVariable, getLabel, labeledUnits=sensorAttributeLookup(input$sensor, units)))))+
        geom_point()+
        scale_x_datetime(date_labels = "%Y-%m-%d\n%H:%M")+
        theme_bw() + 
        theme(panel.border = element_blank(),
              panel.grid.major = element_blank(),
              plot.title = element_text(hjust = 0.5),
              panel.grid.minor = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_text(size=16),
              axis.text = element_text(size=14),
              legend.position="bottom", # position of legend or none
              legend.direction="horizontal", # orientation of legend
              legend.title= element_blank(), # no title for legend
              legend.key.size = unit(0.5, "cm"), # size of legend
              legend.text = element_text(size=14),
              axis.line.x = element_line(color="black", size = 1),
              axis.line.y = element_line(color="black", size = 1))+
        guides(colour = guide_legend(override.aes = list(size=3)))
      
    }
  })
  
  # Generate an HTML table view of the data ----
  output$table <- DT::renderDataTable(DT::datatable({
    sensorDataTimePeriod()
  }))
  
  
  # reactive function return the lastest record
  latestRecord <- reactive({
    d <- sensorData() %>% arrange(Timestamp) %>% tail(1)
    return(d)
  })
  
  
  # render text with the data from the latest record collected
  output$latestTime <- renderUI({
    r <- latestRecord()
    str1 <- paste("<h2>", r$Timestamp, "</h2>")
    HTML(str1)
  })
  
  # Generate an HTML table view of the data ----
  output$table2 <- renderTable({
    ignore_cols <- sensorAttributeLookup(input$sensor, ignore)[[1]] # grab the column names to ignore from the sensor table
    d <- latestRecord()[!names(latestRecord()) %in% ignore_cols]
    
    d %>% rownames_to_column %>% 
      tidyr::gather(var, value, -rowname) %>% 
      tidyr::spread(rowname, value) #%>% 
      #rowwise() %>% mutate(var = getLabel(var))
  }, hover=TRUE, bordered=TRUE, colnames=FALSE)
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      lower <- format(TimePeriodMinMax()[1], "%Y%m%d%H%M")
      upper <- format(TimePeriodMinMax()[2], "%Y%m%d%H%M")
      paste(input$sensor, "_", lower, "-", upper, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sensorDataTimePeriod(), file, row.names = FALSE)
    }
  )
  
  
}

# Create Shiny app ----
shinyApp(ui, server)