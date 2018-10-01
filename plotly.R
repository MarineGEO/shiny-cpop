#plotly dev
source("functions.R")
source("dataSource.R")

library(plotly)

sensor <- "SERC-WaterLevel"
parameter <- "Temperature"

d <- loadSensorCSV(sensorAttributeLookup(sensor, 'urlpath')) %>% tail(500)



p <- d %>%
  ggplot(aes_string(x="Timestamp", y=parameter, colour=parameter, label=parameter))+
  geom_point(aes_string(x="Timestamp", y=parameter))+
  ylab(paste(getLabel(parameter, sensorAttributeLookup(sensor, units)), "\n\n"))+
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

ggplotly(p, dynamicTicks = F)



p <- plot_ly(data = d,x = ~Timestamp, y = ~Temperature, type="scatter", mode = 'markers', marker = list(size = 5,
                                                                                            color = 'rgba(54, 127, 169, 1)',
                                                                                            line = list(color = 'rgba(54, 127, 169, .8)',
                                                                                                        width = 1)))
p
