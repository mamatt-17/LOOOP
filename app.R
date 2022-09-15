# Load packages----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
#library(rsconnect)
library(leaflet)
library(rstudioapi)
library(shinycssloaders)
library(rmarkdown)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)
library(htmltools)
library(fontawesome)

# Choices for drop downs----
param_choices <- c("Temperature (deg. C)"="Temp", "Specific Conductance (uS/cm)"="SC", "pH (units)"="pH", "Dissolved Oxygen (mg/L)"="DO", "Turbidity (NTU)"="Tn", "Chlorophyll-a (ug/L)"="Chl")

sites <- c("Select a Station","B143", "B148", "B211", "B22", "B224", "B266", "B317", "B409", "B430", "CROSS")

depth_choices <-list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)


# Create User Interface (UI)----
ui <- navbarPage("LOOOP",
                 tabPanel("Data Explorer",
                          fluidRow(
                             box(plotOutput("plot1", height = 350)),
                             box(title = "Controls",
                                 selectInput("param_choices","Parameter:", param_choices),
                                 selectInput("site_choices", "Station:", sites),
                                 selectInput("depth","Depth:", NULL),
                                 uiOutput("date_slider")),
                             leaflet::leafletOutput("mymap")
                           )
                          ),
                 tabPanel("Explorer Guide",
                          includeMarkdown("StaticPosts/About-Data.Rmd")
                          ),
                 tabPanel("Water Quality Parameters",
                          includeMarkdown("StaticPosts/Parameter_Descriptions.Rmd")
                          ),
                 tabPanel("About LOOOP",
                          includeMarkdown("StaticPosts/Credits-Policies.Rmd")
                          ),
                 collapsible = TRUE,
                 position = "static-top")

# Create server function (response to UI)----
server <- function(input, output, session){
  showModal(modalDialog(title = "Welcome to the LOOOP!",
                        "Use the Plot Options to begin exploring data collected by the Upstate Freshwater Institute.
                        Click outside of this box and select a Station to get started!",
                        size = "l",
                        easyClose = T,
                        footer = NULL))
            
load("riverdata.rdata")
load("mapframe.rdata")
  
  site_choices <-reactive({
    return(subset(river.data, Station == input$site_choices))
  })
  
  observeEvent(site_choices(),{
    d.choices <- unique(na.omit(site_choices()$Depth))
    updateSelectInput(inputId = "depth", choices = d.choices)
  })
  
  depth <- reactive({
    req(input$depth)
    filter(site_choices(), Depth == input$depth)
  })
  
  output$date_slider <- renderUI({
    req(input$site_choices)
    date_min= min(site_choices()$Datetime)
    date_max = max(site_choices()$Datetime)
    sliderInput("date_slider","Date Range:", min = date_min, max = date_max, value = c(date_min,date_max),timezone = "EST")
  })
  
  
  our.data <- reactive({
    return(subset(river.data, (params %in% input$param_choices & Station %in% input$site_choices & Depth %in% input$depth | is.na(Abs.Depth))))
   return(subset(river.data, (Datetime >= input$date_slider[1] & Datetime <= input$date_slider[2])))
  })
  

  output$plot1 <- renderPlot({
    ggplot(data = our.data(), mapping = aes(x = Datetime, y = value))+
      geom_point(size = 2)+
      geom_line()+
      theme_minimal()+
      labs(x = "Date", y = names(param_choices[which(param_choices == input$param_choices)]))+
      scale_x_datetime(limits = c(input$date_slider[1],input$date_slider[2]),
                       breaks = breaks_pretty(),
                       labels = label_date_short(format = c("%Y", "%b", "%d", "%H:%M"), sep = "\n"))+
      theme(
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      axis.ticks = element_line(color = "black", size = 1),
      axis.text = element_text(size = 15)
      )
  })
  
  mymap <- createLeafletMap(session,"mymap")
  session$onFlushed(once = T, function(){
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.Terrain,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>% setView(
          lng = -76.354,
          lat= 43.248,
          zoom = 9
        ) %>% addCircleMarkers(
          lng = mapframe$long,
          lat = mapframe$lat,
          popup = paste0(
            "Station: ",mapframe$Station, "<br>",
            "Depth(s): ",mapframe$U.Depths, "<br>",
            "Year(s): ",mapframe$U.Years),
          labelOptions = labelOptions(textsize = "15px"))
    })
  })
  
  }


## Run app----
shinyApp(ui = ui, server = server)


##DEPLOY APP to shinyapps.io server----
#library(rsconnect)
#rsconnect::deployApp("\\\\aquadog/analysis/2021_LOOOP_NYSG_Small_Grant/06_Webpage-Shiny App/LOOOP")


