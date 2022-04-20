# Shiny File for running LOOOP page
# You can run the application by clicking the 'Run App' button above; large application so it may not load
# Note, when the application is running you have to press the Stop button in order to run additional code.

# Use shinyApp() to print application; images may not load using this function



# Load packages and Set Working Directory----
#devtools::install_github("utah-dwq/wqTools")
my_packages <- c("lubridate", "plyr", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2","leaflet", "ggvis", "RSQLite","knitr","wqTools")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Initial Setup for Application----

# Load data files from initial data workup file (looop_data)

param_choices <- c("Temp", "SC", "pH", "DO", "Tn", "Chl")
names(param_choices) <- c("Temperature", "Specific Conductance","pH","Dissolved Oxygen","Turbidity","Chlorophyll-a")

#site_choices <- c("B143", "B148", "B211", "B22", "B224", "B266", "B317", "B409", "B430", "CROSS")

#depth_choices <-list("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5, "6" = 6, "7" = 7)

# Create User Interface (UI)----
ui <-fluidPage(
# Title that will appear at top of page
  navbarPage(title = div(
    #style = "padding: 1px 0px; width: '100%'",
    img(src = "logo_looop.png", width = "20px",height = "20px"),
    "LOOOP-Lake Ontario, Oneida, Onondaga Program"
    ), 

  # Data Explorer Interactive Application tab (sub-tabs on side panel)----
  tabPanel("Data Explorer", icon = icon("chart-bar"), 

# Adding in side-panel navigational sub-tabs
navlistPanel("Data Explorer",
# First side-panel navigational tab (Application)
             tabPanel("Application",
# Setting up interface using fluid grid system (12 columns across)
                      fluidRow(
# In columns 1-3, map of area with clickable data points will appear
  column(3, h4("Click a site for more information"),
         shinycssloaders::withSpinner(leaflet::leafletOutput("mymap"))
         ),

# In columns 4-7, data filter/inputs for graphing will appear
  column(3,
         wellPanel(
# Overall Title
           h4("Plot options"),
# Selection for Station
selectInput("site_choices", label = "Station:",
            choices = unique(b3$Station,)
),
# Selection for Parameter
           selectInput("param_choices", label = "Parameter:",
                       choices = param_choices
                       ),
# Selection for Depths
selectInput("depth", label = "Depth(s):",
                   choices = NULL),
# Time Range Slider
uiOutput("date_slider"),
#checkboxInput("show_dates", label = "Show all profile dates", value = TRUE)
),
uiOutput("unique_dates")
),
# Output from selections, in columns 8-12
column(5,
# Plot graph
       plotOutput("plot"))
)
), # End of Application Sub-tab

# Second side-panel navigational tab (User's Guide)
tabPanel("User's Guide",
         h4(p("Coming Soon"))
         ), # End of How-To Sub-tab

# Third side-panel navigational tab (Meta Data)
tabPanel("About the Data",
         h4(p("Coming Soon"))
         ), # End of Meta sub-tab
# Set side-panel width (2) and Nav list panel (10) widths using fluid grid system
fluid = TRUE, widths = c(2,10))), # End of Data Explorer tab

  # Lake Characteristics navigational (expandable) tab----
navbarMenu("Lake Characteristics", icon = icon("water"),
# Options beneath "Lake Characteristics"
           tabPanel("Lake Ontario",
                    h4(p("Coming Soon"))
                    ),
           tabPanel("Oneida Lake",
                    h4(p("Coming Soon"))
                    ),
           tabPanel("Onondaga Lake",
                    h4(p("Coming Soon"))
                    )
           ), # End of Lake Characteristics tab

  # Topics navigational (expandable) tab----
navbarMenu("Topics", icon = icon("lightbulb"),
# Options beneath "Topics"
           tabPanel("Great Lakes Geomorphology",
# Read R markdown file that contains text and images
                    includeMarkdown("StaticPosts/Geomorph.Rmd")),
           tabPanel("Lake Levels",
                    includeMarkdown("StaticPosts/LakeLevels.Rmd")),
           tabPanel("Land Use",
                    includeMarkdown("StaticPosts/LandUse.Rmd")),
           tabPanel("Fisheries",
                    h5(p("Coming Soon"))),
           tabPanel("Effects of Pollution",
                    includeMarkdown("StaticPosts/Pollution.Rmd"))
           ), # End of Topics tab

  # About LOOOP tab----
tabPanel("About LOOOP", icon = icon("info-circle"), 
fluidRow(column(6,
                includeMarkdown("StaticPosts/About.Rmd")),
         column(6,align = "center",
                img(src = "logo_looop.png", width = "400px",height = "400px"))
         )
), # End of About tab

  # Credits and Privacy Policy tab----
tabPanel("Credits, Policy and Contact Information", icon = icon("question-circle"),
         h4(p("Coming Soon"))
) #End of Policy tab
) # End of NavBarpage
) # End of Fluid Page

# Create server function (response to UI)----
server <- function(input, output, session){
  
# Create Dialog Box to keep user from reloading page
  showModal(modalDialog(title= "MAP LOADING","Please wait for map to draw before proceeding.",size = "l",footer = NULL))

  # Remove Loading Dialog Box when map has been drawn
  observe({
    req(mymap)
    removeModal()
  })

# Set up app with data and defining objects----
  #load("~/GitHub/LOOOP/looop.rdata")
  load("//aquadog/analysis/2021_LOOOP_NYSG_Small_Grant/06_Webpage-Shiny App/LOOOP/looop.rdata")
  
# Defining reactive objects
  
  reactive_objects = reactiveValues()
  
  
# Map Set Up----
  mymap <- createLeafletMap(session, "mymap")

# Draw Map with markers at data sites, set View of map to area of interest upon loading
session$onFlushed(once= T, function(){
  output$mymap <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>% setView(
      lng = -76.354,
      lat= 43.248,
      zoom = 9
    ) %>% addCircleMarkers(
      lng = b3$long,
      lat = b3$lat,
      popup = paste0(
                    "Station: ",b3$Station, "<br>",
                    "Depth(s): ",b3$U.Depths, "<br>",
                    "Year(s): ",b3$U.Years),
      labelOptions = labelOptions(textsize = "15px")
    )
})

  })

# Reactivity----
# Filtering depth choices based on selected site
site_choices <- reactive({
  filter(b5, Station == input$site_choices)
})
observeEvent(site_choices(),{
  choices <- unique(site_choices()$Depth)
  updateSelectInput(inputId = "depth", choices = choices)
})

depth <- reactive({
  req(input$depth)
  filter(site_choices(), Depth == input$depth)
})


# When station is selected, date slider that controls the X axis appears
  output$date_slider <- renderUI({
    req(input$site_choices)
    date_min = min(site_choices()$Date)
    date_max = max(site_choices()$Date)
    sliderInput("date_slider","Date range:", min = date_min, max = date_max, value = c(date_min,date_max),timeFormat = "%m-%d-%Y")
  })

# Create new data frame that reacts to user selections
  selectedData <- reactive({
    b5[c(input$site_choices,input$param_choices, input$depth),]
  })
  
  
  # Create a timeseries plot based on the selected options----  
  output$plot = renderPlot({
    
        ggplot(data = selectedData(), mapping = aes(x = Datetime, y = value))+
          geom_point(size = 2)+
          geom_line()+
          theme_minimal()+
          #scale_y_continuous(name =  name,
           #                  limits = c(floor(min(b6$value, na.rm = T)),ceiling(max(b6$value,na.rm = T))),
            #                 breaks = c(seq(floor(min(b6$value, na.rm = T)),ceiling(max(b6$value,na.rm = T))),.5))+
        #  scale_x_datetime(name = "Date",
         #                  date_breaks = "1 month",
          #                 date_labels = "%b %y",
           #                date_minor_breaks = "1 day")+
          theme(
            panel.border = element_rect(color = "black", fill = NA, size = 1),
            axis.ticks = element_line(color = "black", size = 1),
            axis.text = element_text(size = 12)
          )
      })
}


  

## Run app----
shinyApp(ui = ui, server = server)
#runApp()
