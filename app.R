# Load packages----
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rsconnect)
library(leaflet)
library(rstudioapi)
library(shinycssloaders)
library(rmarkdown)
library(markdown)
library(lubridate)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(scales)
library(htmltools)
library(fontawesome)
library(DT)



# Create User Interface (UI)----
ui <- navbarPage("LOOOP",
                 header = (includeHTML(("google-analytics.html"))),
                 tabPanel("Data Explorer",
                          fluidRow(
                             box(uiOutput("plotui")),
                             box(title = "Controls",
                                 selectInput("data_type", "Monitoring Type:", choices = list('Stream Survey' = "s", 'Weather Station' = "w"), selected = "w"),
                                 selectInput("site_choices", "Site:", choices = NULL),
                                 selectInput("param_choices","Parameter:", choices = NULL),
                                 uiOutput("date_slider"))
                           ),
                          fluidRow(
                            box(h4("Points within box"),
                                   DT::dataTableOutput("brush_info")
                            ),
                            box(leaflet::leafletOutput("mymap")
                          ))
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
                        "Use the Plot Options to begin exploring data collected by students and youth in Central New York.
                        Click outside of this box and select a dropdown option to get started!",
                        size = "l",
                        easyClose = T,
                        footer = NULL))
            
load("student.rdata")

# Create reactive control box options based on selected dataset (stream surveys or weather station)
datasetInput <- reactive({
  if(input$data_type == "s"){
    selected.dataset <- s.long
  }
  else if(input$data_type == "w"){
    selected.dataset <- w.long
  }
return(selected.dataset)
  })

# Using reactive dataset element, update dropdown choices for sites and parameters
observeEvent(datasetInput(),{
  s.choices <- unique(na.omit(datasetInput()$Site.Name))
  updateSelectInput(session,inputId = "site_choices", "Site:", choices = s.choices, selected = NULL)
})

observeEvent(datasetInput(),{ 
  p.choices <- unique(na.omit(datasetInput()$params))
  updateSelectInput(session, inputId = "param_choices", "Parameter:", choices = p.choices, selected = NULL)
})

# Using reactive dataset, update date range to min and max of dataset and filtered site
output$date_slider <- renderUI({
  req(input$site_choices)
  date_min= min(datasetInput()$Datetime[datasetInput()$Site.Name == input$site_choices])
  date_max = max(datasetInput()$Datetime[datasetInput()$Site.Name == input$site_choices])
  sliderInput("date_slider","Date Range:", min = date_min, max = date_max, value = c(date_min,date_max), step = 3600, timezone = "EST")
}) 

# Create a reactive dataframe that subsets the selected dataset based on selected choices for graphing
our.data <- reactive({
  if(input$data_type == "s"){
    return(subset(s.long, (params %in% input$param_choices & Site.Name %in% input$site_choices
                           & Datetime >= input$date_slider[1] & Datetime <= input$date_slider[2])))
  }
  else if(input$data_type == "w"){
    return(subset(w.long, (params %in% input$param_choices & Site.Name %in% input$site_choices
                           & Datetime >= input$date_slider[1] & Datetime <= input$date_slider[2])))
  }
})

# Create interactive plot
output$plotui <- 
  renderUI({
  plotOutput("plot", height = 350,
             brush = brushOpts(
               id = "plot_brush"
             ))
})

# Make reactive elements for plotting purposes
plottinginfo <- reactive({
  if(input$data_type == "s"){
    majorbreaks <- "day"
    minorbreaks <- "day"
    datelabels <- "%b %d"
  }
  else if(input$data_type == "w"){
    majorbreaks <- "day"
    minorbreaks <- "1 hour"
    datelabels <- "%b %d"
  }
  return(data.frame(majorbreaks, minorbreaks, datelabels))
})

# Plotting framework
output$plot <- renderPlot({
  ggplot(data = our.data(), mapping = aes(x = Datetime, y = Result))+
        geom_point(size = 2)+
        geom_line()+
        theme_minimal()+
        labs(x = "Date", y = input$param_choices)+
        scale_x_datetime(limits = c(input$date_slider[1],input$date_slider[2]),
                         breaks = plottinginfo()$majorbreaks,
                         minor_breaks = plottinginfo()$minorbreaks,
                         labels = date_format(plottinginfo()$datelabels, tz = "America/New_York")
        )+
        theme(
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          axis.ticks = element_line(color = "black", linewidth = 1),
          axis.text = element_text(size = 15)
        )
    })

# Make reactive elements for brush table
tableinfo <- reactive({
  if(input$data_type == "s"){
    selectedinfo <- 'Macroinvertebrates Present'
  }
  else if(input$data_type == "w"){
    selectedinfo <- "Wind Direction"
  return(selectedinfo)
  }
})

# Make data table that contains information related to what is boxed in plot
output$brush_info <- DT::renderDataTable({
  res <- brushedPoints(our.data() %>% select(., c(Datetime, Group, Latitude, Longitude, tableinfo(), Result)), input$plot_brush)
  datatable(res)
})

# Making the map
mymap <- createLeafletMap(session,"mymap")

output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stadia.StamenTerrain,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>% setView(
        lng = -76.176684,
        lat= 43.082970,
        zoom = 10
      ) %>% addCircleMarkers(
        lng = s.long$Longitude,
        lat = s.long$Latitude,
        color = "blue",
        popup = paste0(
          "Location: ",s.long$Location, "<br>",
          "Latest Monitoring Date: ", format(s.long$Datetime, "%m/%d/%Y"), "<br>",
          "Group(s): ",s.long$Group),
        labelOptions = labelOptions(textsize = "15px")
        ) %>% addCircleMarkers(
      lng = w.long$Longitude,
      lat = w.long$Latitude,
      color = "red",
      popup = paste0(
        "Location: ",w.long$Site.Name, "<br>",
        "Latest Monitoring Date: ", format(w.long$Datetime, "%m/%d/%Y"), "<br>",
        "Group(s): ",w.long$Group),
      labelOptions = labelOptions(textsize = "15px")
        )
  })




}
## Run app----
shinyApp(ui = ui, server = server)


##DEPLOY APP to shinyapps.io server----
#library(rsconnect)
#rsconnect::deployApp("\\\\aquadog/analysis/2021_LOOOP_NYSG_Small_Grant/06_Webpage-Shiny App/LOOOP")


