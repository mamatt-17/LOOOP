# Shiny File for running LOOOP page
# You can run the application by clicking the 'Run App' button above. 
# Arrow next to text allows you to run in Viewer or external window.
# Note, when the application is running you have to press the Stop button in order to run additional code.


# Load packages and Set Working Directory----
my_packages <- c("lubridate", "plyr", "openxlsx", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data and rename Inputs----
data <- read.csv("shiny_data.csv")
data$Input <-as.factor(data$Input)
levels(data$Input) <-list("1" = "Input 1", "19" = "Input 19", "20" = "Input 20", "32"= "Input 32")

# Define UI (user interface) for application that draws a plot based on selected inputs----
ui <- fluidPage(
  
  # Application title
  titlePanel("DeRuyter Tributaries 2020"),
  
  # Create a sidebar that will contain our interactive features
  fluidRow(
    column(width = 3,wellPanel(
      # Create a select box with DeRuyter Lake Inputs as our choices 
      selectInput(inputId = "trib", "Input No.",
                  choices = c("1", "19", "20", "32"),
                  selected = 1),
      # Create another select box with Lab analyses as our choices
      radioButtons(inputId = "nutrient", "Lab Parameter",
                   choices = c("TN","TP","TSS"),
                   selected = "TN"))),
    
    # On the same line as the selection features, add a box that will give information of point when clicked
    fluidRow(
      column(width = 7,
             h4("Points near click"),
             verbatimTextOutput("click_info")))
  ),
  # Based on selection features, show a plot of the data for that tributary upon selection and be able to click on points
  fluidRow(
    column(width = 12,
           plotOutput("nutrientPlot", height = 350,
                      click = "plot_click")
    )),
  fluidRow(
    column(width = 7,offset = 5,
           img(src = "UFI_HighResLogo.png", height =100, width =190)
    ))
)


# Define server logic required to draw the plot----
server <- function(input, output) {
  
  # Create the output plot (as named in our UI)
  output$nutrientPlot <- renderPlot({
    # Create a reactive data frame that responds to interactive selections
    Input_filter<- reactive({
      data <-data %>% filter(
        Input == input$trib) %>%
        filter(
          Test == input$nutrient)
    })
    
    # Create dynamic y axis label based on which nutrient is selected
    if(input$nutrient == "TN"){
      ylabel <- "TN (µg/l)"
    } else if(input$nutrient=="TP"){
      ylabel <- "TP (µg/l)"
    } else if(input$nutrient == "TSS")
      ylabel <- "TSS (mg/l)"
    
    # Create dynamic y axis limits based on which nutrient is selected
    if(input$nutrient == "TN"){
      limits <- c(100,2200)
    } else if(input$nutrient=="TP"){
      limits <- c(0,550)
    } else if(input$nutrient == "TSS")
      limits <- c(0,90)
    
    # Create dynamic y axis breaks based on which nutrient is selected
    if(input$nutrient == "TN"){
      breaks <- seq(100,2200,500)
    } else if(input$nutrient=="TP"){
      breaks <- seq(0,550,100)
    } else if(input$nutrient == "TSS")
      breaks <- seq(0,90,15)
    
    # Actual plotting code
    ggplot(data = Input_filter(),aes(x = Average.q,Result, color = "black"))+ # Use the filtered dataframe, x = Flow, y = Result of whatever nutrient is selected
      geom_point(size = 4, color = "black") + # Size and color of scatterplot
      theme_minimal()+ 
      scale_y_continuous(name = ylabel, # dynamic y label
                         limits = limits, # dynamic limits
                         breaks = breaks, # dynamic breaks
                         expand = c(0.1,0.1))+ # Slight cushion around axis so that limits are not "absolutes"
      scale_x_continuous(name = "Flow (cfs)", # X label
                         limits = c(0,3), # x axis limits
                         breaks = seq(0,3,.25), # Ticks from 0 to 3, every 0.25
                         expand = c(.01,.1))+ # Slight cushion around axis so that limits are not limiting data from being shown
      geom_smooth(method = "lm", se = FALSE, color = "blue")+ # Add a linear regression line and make it blue
      stat_regline_equation(label.x.npc = 0.02,label.y.npc = "top", # Add the Regression equation 2% from left and at the top of the plot
                            aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~~~")), color = "black", size = 8)+ # Paste equation and adjusted R squared
      stat_cor(label.x.npc = 0.02, label.y.npc = 0.8, aes(label = paste(..p.label..)), color = "black", size = 8)+ # Paste p value 2% from left and 80% from bottom of plot
      theme(
        legend.position = "none", # Do not show legend
        panel.border = element_rect(color = "black", fill = NA, size = 1), # Create a border around the plot
        axis.ticks = element_line(color = "black", size = 1), # Add tickmarks so they can be seen
        axis.text = element_text(size = 14), # Make axis text size 14
        axis.title = element_text(size = 14)
      )
  })
  # Print the data frame for the nearest selected points
  output$click_info <- renderPrint({
    nearPoints(data,input$plot_click, addDist = F, threshold = 3)
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)

