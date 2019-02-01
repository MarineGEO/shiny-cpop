# Data sources
library(tibble)

# Specify list of sensors to include in the shiny app.
# Must include the site, sensor name (suggested format is site-type "SERC-WaterLevel"), public web accessible URL, columns to ignore, units(?)

waterquality.labels <- c("Temperature"="Temperature (°C)", 
           "Specific.Conductivity"="Specific Conductivity (mS/cm)",
           "Conductivity"= "Conductivity (mS/cm)",
           "Salinity"= "Salinity (psu)",
           "Dissolved.Oxygen"= "DO (%)",
           "Dissolved.Oxygen.2"= "DO (mg/l)",
           "Pressure"= "Pressure (psi)",
           "Depth"= "Depth (meters)",
           "Turbidity"= "Turbidity (FNU)",
           "Chlorophyll"= "Chlorophyll (RFU)",
           "Chlorophyll.2"= "Chlorophyll (µg/l)",
           "BGA.PE"= "BGA-PE (RFU)",
           "BGA.PE.2"= "BGA-PE (µg/l)",
           "FDOM"= "FDOM (RFU)",
           "FDOM.2"= "FDOM (ppb QSE)",
           "Battery"= "Battery (Volts)")

met.labels <- c("Air.Temperature"="Air Temperature (°C)",
           "Barometric.Pressure"="Barometric Pressure (mbar)",
           "Daily.Rain"="Daily Rain (mm)",
           "PAR"="PAR",
           "Rain.Rate"="Rain Rate (mm/hr)",
           "Relative.Humidity"="Relative Humidity (%)",
           "Wind.Direction"="Wind Direction (°)",
           "Wind.Gust"="Wind Gust (mph)",
           "Wind.Speed"="Wind Speed (mph)"
           )

waterlevel.labels <- c("Air.Temperature"="Air Temperature (°C)",
                "Barometric.Pressure"="Barometric Pressure",
                "Daily.Rain"="Daily Rain ()",
                "PAR"="PAR",
                "Rain.Rate"="Rain Rate",
                "Relative.Humidity"="Relative Humidity",
                "Wind.Direction"="Wind Direction",
                "Wind.Gust"="Wind Gust",
                "Wind.Speed"="Wind Speed"
)

# R dataframe of sensors to include in the app. Must be called sensors.
sensors <- tribble(
  ~site, ~sensorName, ~type, ~urlpath, ~ignore, ~na, ~units, ~coordinates, 
  "SERC", "SERC-WaterQuality", "waterquality", "https://dl.dropboxusercontent.com/s/n8sagrl8iwdmktm/2017_Water_Quality_RAW_SERC.csv?dl=0", c("Timestamp", "Site", "Bat"),c("", "NA"), waterquality.labels, c(38.886179, -76.541424),
  "SERC", "SERC-MET", "met", "https://dl.dropboxusercontent.com/s/rnkq0i0r7uxc5l5/2017_MET_RAW_SERC.csv?dl=0", c("Timestamp", "Site"),c("", "NA", "-Invalid-"), met.labels, c(38.886179, -76.541424),
  "SERC", "SERC-WaterLevel", "waterlevel", "https://dl.dropboxusercontent.com/s/byukiwxjtxkw0nm/2017_Water_Level_RAW_SERC.csv?dl=0", c("Timestamp", "Site", "Status"),c("", "NA"), waterlevel.labels, c(38.886179, -76.541424)
)
