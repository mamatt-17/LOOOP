# Script for cleaning and formatting data to be used with LOOOP Shiny page

# Set working directory and load packages----
my_packages <- c("lubridate", "plyr", "openxlsx", "dplyr","ggpubr",  "tidyr", "shiny","ggplot2")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in csv files of each dataset (a. Onondaga Lake buoy, b. 3 Rivers buoy data, c. Lake Ontario buoys data, d. Oneida Lake)----
    # A. Onondaga Lake data----

    # B. 3 Rivers data, read all files into 1 dataset----
filenames <- list.files(path = './CSV_files/3Rivers', pattern = '*', full.names = TRUE)
b <- ldply(filenames, read.csv)

    # C. Lake Ontario data----

    # D. Oneida Lake data----

# Manipulated data so that it meets requirements for visualizations----
    # A. Onondaga Lake----

    # B. 3 Rivers----
b$system.code <- as.character(b$system.code)
b$date <- as.Date(b$date,format = "%m/%d/%Y")
b$datetime <- as.POSIXct(paste(b$date, b$time), format = "%Y-%m-%d %H:%M") # Time cannot exist without date

    # Adding time period factor based on collection time (1. 21:00-3:00, 2.  3:01-9:00, 3. 9:01-15:00, 4. 15:01-21:00)
b$timeperiod <- cut(hour(b$datetime), breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 24), include.lowest = TRUE)
b <- b %>% 
mutate(tp = lapply(timeperiod, 
                           function(y) 
                             if(y == "[0,3]"  | y == "(21,24]" ) 
                               1 
                           else 
                               if(y == "(3,6]"  | y == "(6,9]")
                                 2 
                             else 
                                 if(y == "(9,12]"  | y == "(12,15]" ) 
                                   3 
                               else 4))
b$tp <- as.factor(as.character(b$tp))
    
    # Factor stations and add coordinates
b$station.code <- as.factor(as.character(b$station.code))
buoy.coord <-as.data.frame(unique(b$station.code))
names(buoy.coord) <-c("station.code")
buoy.coord$lat <-c(43.440481,43.44995,43.205123,43.231178,43.193388,43.139376,43.147516,43.103883,43.100510,43.108055)
buoy.coord$long <- c(-76.490645,-76.50349,-76.269799,-76.309791,-76.279911,-76.238025,-76.314771,-76.445725,-76.499537,-76.475456)
    
# C. Lake Ontario----

    # D. Oneida Lake----   
    
    