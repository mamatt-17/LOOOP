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

# Choices for drop downs----
param_choices <- c("Temperature (deg. C)"="Temp", "Specific Conductance (uS/cm)"="SC", "pH (units)"="pH", "Dissolved Oxygen (mg/L)"="DO", "Turbidity (NTU)"="Tn", "Chlorophyll-a (ug/L)"="Chl")

sites <- c("B143", "B148", "B211", "B22", "B224", "B266", "B317", "B409", "B430", "CROSS")

depth_choices <-list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)

# Create User Interface (UI)----
ui <- dashboardPage(
    dashboardHeader(title = "LOOOP"),
    dashboardSidebar(menuItem("Data Explorer",tabName = "Data", icon = icon("chart-bar")),
                     menuItem("About the Data", tabName = "Meta"),
                     menuItem("Topics", tabName = "Topics", icon = icon("lightbulb")),
                     menuItem("About LOOOP", tabName = "About", icon = icon("question-circle"))),
    dashboardBody(tabItems(
      tabItem(tabName = "Data",
      fluidRow(
        box(plotOutput("plot1", height = 250)),
        box(title = "Controls",
            selectInput("param_choices","Parameter:", param_choices),
            selectInput("site_choices", "Station:", sites),
            selectInput("depth","Depth:", NULL)
            ,
            uiOutput("date_slider")
            ) #box
      )), # Row/data tab
      tabItem(tabName = "Meta",
      h2("coming soon")),
      tabItem(tabName = "Topics",
      h2("coming soon")),
      tabItem(tabName = "About",
      h2("coming soon"))
    ) # tab Items
    ) # Dash Body
  )# Dash Page

# Create server function (response to UI)----
server <- function(input, output, session){

load("riverdata.rdata")
  
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
      axis.text = element_text(size = 20)
      )
  })
  }


## Run app----
shinyApp(ui = ui, server = server)


##DEPLOY APP to shinyapps.io server----
#library(rsconnect)
#rsconnect::deployApp("\\\\aquadog/analysis/2021_LOOOP_NYSG_Small_Grant/06_Webpage-Shiny App/LOOOP")


