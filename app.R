# Shiny File for running LOOOP page
# You can run the application by clicking the 'Run App' button above. 
# Arrow next to text allows you to run in Viewer or external window.
# Note, when the application is running you have to press the Stop button in order to run additional code.


# Load packages and Set Working Directory----
my_packages <- c("lubridate", "plyr", "openxlsx", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2","leaflet")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

coord <-readOGR(dsn="Coordinates.shp",layer = "station.co", encoding = "UTF-8")
mcoord<-read.csv("Coordinates.csv")
coordinates <- SpatialPointsDataFrame(mcoord[,c('Longitude', 'Latitude')] , mcoord)


ui <-fluidPage(
  headerPanel("LOOOP - Lake Ontario, Oneida, Onondaga Program"),
  leafletOutput("map")
)
server <- function(input, output, session){
  
  # Loading modal to keep user out of trouble while map draws...
  showModal(modalDialog(title="MAP LOADING - PLEASE WAIT...","Please wait for map to draw before proceeding.",size="l",footer=NULL))

  # Draw map
  output$map <-renderLeaflet({
    leaflet %>%
      addTiles() %>%
      addCircles(lat = mcoord$lat,
                 lng = mcoord$long)

  })
  }

## run app----
shinyApp(ui = ui, server = server)
