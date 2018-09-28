# R code to produce ggplot2 figures for realtime sensors visualization

# create a plot showing a timeseries for a single parameter from a sensor dataset
plotSingleParameter <- function(data, selectedVariable, timestampField=Timestamp){

  selectedVariable <- enquo(selectedVariable)
  timestampField <- enquo(timestampField)
  print(selectedVariable)
  print(timestampField)
  p <-  data %>% 
        #ggplot(aes_string(x="Timestamp", y=input$selectedVariable, colour=input$selectedVariable))+
        ggplot(aes(x=!!timestampField, y=!!selectedVariable, colour=!!selectedVariable))+
        #geom_point(aes_string(x="Timestamp", y=input$selectedVariable))+
        geom_point(aes(x=!!timestampField, y=!!selectedVariable))+
        #ylab(getLabel(!!selectedVariable))+
        #ylab(getLabel(input$selectedVariable, sensorAttributeLookup(input$sensor, units)))+
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
  p
  
}  


d <- loadSensorCSV(sensorAttributeLookup("SERC-WaterQuality", urlpath))
d %>% plotSingleParameter("Temperature")

