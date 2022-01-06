# Shiny File for running LOOOP page
# You can run the application by clicking the 'Run App' button above; large application so it may not load
# Note, when the application is running you have to press the Stop button in order to run additional code.

# Use shinyApp() to print application; images will not load using this function


# Load packages and Set Working Directory----
my_packages <- c("lubridate", "plyr", "openxlsx", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2","leaflet", "ggvis", "RSQLite","knitr")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Initial Setup for Application----

# Load data files from initial data workup file (looop_data)
source("looop_data.R")


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
# In columns 1-5, map of area with data points will appear
  column(5,
         leafletOutput("mymap")
         ),

# In columns 6-8, data filter/inputs for graphing will appear
  column(3,
         wellPanel(
# Overall Title
           h4("Filter"),
# Time Range Slider
           sliderInput("year","Year(s)",2000,2010,
                       value = c(2001, 2009), sep = ""),
# Checkboxes for Sites
           checkboxGroupInput("site","Site(s)",
                       choices = list("B148"= "B148","B211"="B211","B143"="B143","B22"="B22","B224"="B224","B266"="B266","B317"="B317","B409"="B409","B430"="B430","CROSS"="CROSS"),
                       selected = 1
                       ),
# Checkboxes for Depths
           checkboxGroupInput("depth","Depth(s) (meters)",
                      choices = list("1"=1,"2"=2, "3" = 3, "4" = 4, "5" = 5, "6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16),
                      selected = 1
           )
)
),
# Output from selections, in columns 9-12
column(4,
# Plot graph
       #ggvisOutput("plot1"),
# Print text of selected objects
       verbatimTextOutput("sitename"),
       verbatimTextOutput("depths")
       )
)
), # End of Application Sub-tab

# Second side-panel navigational tab (User's Guide)
tabPanel("User's Guide",
         h4(p("Coming Soon"))
         ), # End of How-To Sub-tab

# Third side-panel navigational tab (Meta Data)
tabPanel("Metadata",
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
         column(6,
                img(src = "~/logo_looop.png", width = "20px",height = "20px"))
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
  #vis <- reactive({
   # yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    #xvar_name <- names(axis_vars)[axis_vars ]
    #yvar <- prop("y",as.symbol(input$yvar))
    
#  data %>%
 #   ggvis(x = xvar, y = yvar) %>%
  #  layer_points(size := 50, size.hover := 200,
   #              fillOpacity := 0.2, fillOpacity.hover := 0.5,
    #             stroke = ~System) %>%
    #add_tooltip(ttip, "hover") %>%
    #add_axis("y", title = yvar_name) %>%
    #add_legend("stroke", title = "System", values = c("ONR","OSWR","SR"))%>%
    #set_options(width = 500,height = 500)
    #})
  #vis %>% bind_shiny("plot1")
output$mymap <- renderLeaflet({
  leaflet() %>%
    addProviderTiles(providers$Stamen.Terrain,
                     options = providerTileOptions(noWrap = TRUE)
    ) %>% setView(
      lng = -76.354,
      lat= 43.248,
      zoom = 9
    ) %>% addCircleMarkers(
      lng = buoy.coord$long,
      lat = buoy.coord$lat,
      popup = buoy.coord$station.code,
      labelOptions = labelOptions(textsize = "15px")
    )
})

  output$sitename <- renderPrint({input$site})
output$depths <- renderPrint({input$depth})
}
## Run app----
shinyApp(ui = ui, server = server)
#runApp()
