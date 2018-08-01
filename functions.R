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
  return(a)
}

# list of all sensors listed in the data.frame
sensorList <- function(){
  # how to grab the list of sensor names
  list_sensorNames <- unique(sensors$sensorName)
  return(list_sensorNames)
}


# loads sensor's data from a csv file 
loadSensorCSV <- function(fullurl){
  sensorData <- readr::read_csv(url(fullurl)) # loads a CSV file from a publicly hosted dataset (ie dropbox, github)
  names(sensorData) <- make.names(names(sensorData)) # prettify the dataset column names (removes spaces + illegal characters)
  return(sensorData)
}

# pretty variable name with units
getLabel <- function(variable){
  # get the pretty label with units if it is defined in the labeled units named list
  label <- tryCatch(labeled_units[[variable]], error=function(cond){return(variable)}) # return function input on error (ie no match)
  return(label)
}

#named list 
#setNames(names(d), lapply(names(d), getLabel))
