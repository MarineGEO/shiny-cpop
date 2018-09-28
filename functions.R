# functions for the cpop shiny app

# Returns the sensor that matches the provided sensor name
sensorLookup <- function(sensorNameLookup){
  u <- sensors %>% filter(sensorName == sensorNameLookup)
  
  # error catching - will stop if multiple or no sensors are matched
  if(nrow(u)==0) warning ('Sensor not found. Make sure that sensor name is defined in sensor dataframe.')
  if(nrow(u)>1) warning ('Multiple sensors returned. Make sure all sensor names are unique.')
  
  # return the matched sensor as a dataframe with 1 row
  return(u)
}

# grabs the attribute value from sensor data table, sensorName lookup should be quoted but attribute does not need to be
sensorAttributeLookup <- function(sensorNameLookup, attribute){
  # see https://dplyr.tidyverse.org/articles/programming.html for info about enquo()
  attribute <- enquo(attribute)
  s <- sensorLookup(sensorNameLookup)
  a <- s %>% pull(!!attribute)
  
  # unnest list by flattening it
  if(typeof(a) == "list"){
    a <- unlist(a)
  }
  
  return(a)
}

# list of all sensors listed in the data.frame
sensorList <- function(){
  # how to grab the list of sensor names
  list_sensorNames <- unique(sensors$sensorName)
  return(list_sensorNames)
}

# list of all sites listed in the data.frame
siteList <- function(){
  list_siteNames <- unique(sensors$site)
  return(list_siteNames)
}


# loads sensor's data from a csv file 
loadSensorCSV <- function(fullurl, na=c("", "NA")){
  sensorData <- readr::read_csv(url(fullurl), na=na, col_types = cols()) # loads a CSV file from a publicly hosted dataset (ie dropbox, github)
  names(sensorData) <- make.names(names(sensorData)) # prettify the dataset column names (removes spaces + illegal characters)
  return(sensorData)
}

# pretty variable name with units
getLabel <- function(variable, labeledUnits){
  variable <- enquo(variable)
  labeledDF <- bind_rows(labeledUnits) # turns named list into a dataframe
  label <- tryCatch(labeledDF %>% select(!!variable) %>% pull(), error=function(cond){return(variable)}) # pull out the units for the selected variable
  return(label)
  }

#named list 
#setNames(names(d), lapply(names(d), getLabel))
