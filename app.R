# Shiny File for running LOOOP page
# You can run the application by clicking the 'Run App' button above. 
# Arrow next to text allows you to run in Viewer or external window.
# Note, when the application is running you have to press the Stop button in order to run additional code.


# Load packages and Set Working Directory----
my_packages <- c("lubridate", "plyr", "openxlsx", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2","leaflet", "ggvis", "RSQLite")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

data <- b2

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href='javascript:void',
         id=inputId,
         class='action-button',
         ...)
}

ui <-fluidPage(
  titlePanel("LOOOP - Lake Ontario, Oneida, Onondaga Program"),
fluidRow(
  column(3,
         wellPanel(
           h4("Filter"),
           sliderInput("year","Year(s)",2000,2010,
                       value = c(2001, 2009), sep = ""),
           checkboxGroupInput("site","Site(s)",
                       choices = list("B148"= "B148","B211"="B211","B143"="B143","B22"="B22","B224"="B224","B266"="B266","B317"="B317","B409"="B409","B430"="B430","CROSS"="CROSS"),
                       selected = 1
                       ),
           checkboxGroupInput("depth","Depth(s) (meters)",
                      choices = list("1"=1,"2"=2, "3" = 3, "4" = 4, "5" = 5, "6"=6,"7"=7,"8"=8,"9"=9,"10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15,"16"=16),
                      selected = 1
                      )
        #wellPanel(
         # selectInput("yvar", "Y-axis variable",axis_vars, selected = "Parameter"),
        # selectInput("xvar", "X-axis variable", axis_vars, selected = "Time")
      #  )
         ))
,
column(9,
       #ggvisOutput("plot1"),
       verbatimTextOutput("sitename"),
       verbatimTextOutput("depths")
       ))
)



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
output$sitename <- renderPrint({input$site})
output$depths <- renderPrint({input$depth})
}
## run app----
shinyApp(ui = ui, server = server)
