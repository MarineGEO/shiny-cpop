# shiny app demo showing SERC water quality, level and met data from early 2017
library(shiny)
library(lubridate)
library(ggplot2)
library(readr)
library(magrittr)
library(dplyr)
#library(scales)

# Source the sensor data list and the functions
source("dataSource.R")
source("functions.R")


function(input, output) {
  
  # UI functions to build the dropdown menus in the shiny app
  # builds UI dropdown selctor for all the data sources at selected site
  output$sensorSelectorUI = renderUI({
    if(input$site==""){return(NULL)}     # checks that the siteSelector is defined
    
    site_sensors <- sensors %>% filter(site==input$site) # filter to just the data sets at the selected site
    sensorList <- unique(site_sensors$sensorName) # TODO see if this can be combined with the filter
    return(selectInput("sensor", "Sensor:", c(sensorList))) # Dropdown Selector for Sensor Dataset
  })
  
  # builds ui dropdown selctor for the sensor variables from all the valid dataframe column names
  output$parameterSelectorUI = renderUI({
    if(input$site=="" | is.null(input$sensor)){return(NULL)}
      ignore_cols <- sensorAttributeLookup(input$sensor, ignore) # grab the column names to ignore from the sensor table
      colnames <- names(sensorData())[!names(sensorData()) %in% ignore_cols] # select all the column names in csv that are not in ignore list
      namedLabels <- setNames(colnames, lapply(colnames, getLabel, labeledUnits=sensorAttributeLookup(input$sensor, units)))
      return(selectizeInput('parameter', 'Parameter', choices=namedLabels, selected=colnames[1]))
  })
  
  # load dataset from external source
  # returns a single sensor dataset when the site and sensor selectors are altered
  sensorData <- reactive({
    if(is.null(input$sensor)){return(NULL)} # check that the sensor has a valid value selected
    cat(file=stderr(), "Switching to", input$sensor, "\n") # log to the console the new sensor type
    d <- loadSensorCSV(sensorAttributeLookup(input$sensor, urlpath), sensorAttributeLookup(input$sensor, na))
    return(d)
  })
  
  # Helper function to filter the sensor dataset to the time of interest
  # function filter dataset to time period (number hours) from the most recent timestamp value
  sensorDataTimePeriod <- reactive({
    d <- sensorData() %>% filter(Timestamp>ymd_hms(max(Timestamp))-hours(input$timeSpan))
    return(d)
  })
  
  # Generate a plot of the data ----
  output$singleParamPlot <- renderPlot({
    if(all(!input$parameter %in% names(sensorData()))){return(NULL)}
    
    sensorDataTimePeriod() %>%
      ggplot(aes_string(x="Timestamp", y=input$parameter, colour=input$parameter))+
      geom_point(aes_string(x="Timestamp", y=input$parameter))+
      ylab(getLabel(input$parameter, sensorAttributeLookup(input$sensor, units)))+
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
  })
  
  # Generate an HTML table view of the data ----
  output$table <- DT::renderDataTable(DT::datatable({
    sensorDataTimePeriod()
  }))
  
  
  # reactive function return the lastest record
  latestRecord <- reactive({
    if(is.null(sensorData())){return(NULL)}
    d <- sensorData() %>% arrange(Timestamp) %>% tail(1)
    return(d)
  })
  
  
  # render text with the data from the latest record collected
  output$latestTime <- renderUI({
    r <- latestRecord()
    str1 <- paste("<h5>", "Last Updated:", r$Timestamp, "</h5>")
    HTML(str1)
  })
  
  # Generate an HTML table view of the data ----
  output$table2 <- renderTable({
    if(is.null(latestRecord())| is.null(input$sensor)){
      return(NULL)
    }
    
    ignore_cols <- sensorAttributeLookup(input$sensor, ignore) # grab the column names to ignore from the sensor table
    d <- latestRecord()[!names(latestRecord()) %in% ignore_cols]
    
    d %>% rownames_to_column %>%
      tidyr::gather(var, value, -rowname) %>%
      tidyr::spread(rowname, value) #%>%
    #rowwise() %>% mutate(var = getLabel(var))
  }, hover=TRUE, bordered=TRUE, colnames=FALSE)
  
  
  # Download sensor dataset for filter time period
  
  # function to get the min/max dates of the filtered dataset
  TimePeriodMinMax <- reactive({
    minDate <- sensorDataTimePeriod() %>% pull(Timestamp) %>% min()
    maxDate <-sensorDataTimePeriod() %>% pull(Timestamp) %>% max()
    return(c(minDate, maxDate))
  })
  
  # Downloadable csv of selected dataset for the desired time period ----
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