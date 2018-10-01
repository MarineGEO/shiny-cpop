# 24 hour report

source("functions.R")
source("dataSource.R")

library(plotly)

sensor <- "SERC-MET"

d <- loadSensorCSV(sensorAttributeLookup(sensor, 'urlpath'), '-Invalid-') %>% tail(24*10)


# summary for the last twenty four hours of data
twentyfour <- d %>% select(-c(sensorAttributeLookup(sensor, 'ignore'))) %>% tidyr::gather("variable", "value") %>% group_by(variable) %>% summarize(Mean=mean(value, na.rm = TRUE), Low=min(value, na.rm = TRUE), High=max(value, na.rm = TRUE), StdDev=sd(value, na.rm = TRUE))
twentyfour


# most recent value
latest <- d %>% tail(1) %>% select(-c(sensorAttributeLookup(sensor, 'ignore'))) %>% rownames_to_column %>% 
  gather(var, value, -rowname) %>% 
  spread(rowname, value) %>% rename("Last"=`1`)


summaryTable <- left_join(latest, twentyfour, by=c("var"="variable"))


maxDate <- d %>% pull(Timestamp) %>% max()
date24 <- maxDate - (24*60*60)

library(lubridate)

t <-d %>% filter(Timestamp>date24) %>% mutate(hour=hour(Timestamp)) %>% 
  select(-c(sensorAttributeLookup(sensor, 'ignore'))) %>% 
  group_by(hour) %>% 
  tidyr::gather("variable", "value", -c(hour)) %>% 
  group_by(hour, variable) %>% 
  summarize(average=mean(value,na.rm=TRUE)) %>% spread(variable, average) %>% 
  mutate("Time"=(date24 + (hour*60*60)))


ymd(maxDate)

ymd_h(maxDate)

View(d %>% filter(Timestamp>date24) %>% mutate(hour=hour(Timestamp)) %>% select(Timestamp, hour))
