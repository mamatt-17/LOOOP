# Script for cleaning and formatting data to be used with LOOOP Shiny page

# Set working directory and load packages----
my_packages <- c("lubridate", "plyr", "dplyr","ggpubr","tidyr")
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
buoy.coord$lat <-c(43.240093,43.44995,43.205123,43.231178,43.193388,43.139376,43.147516,43.103883,43.100510,43.108055)
buoy.coord$long <- c(-76.147568,-76.50349,-76.269799,-76.309791,-76.279911,-76.238025,-76.314771,-76.445725,-76.499537,-76.475456)
#unique(b[c("system.code","station.code")],) # Check that the buoys are in the right river

    # Round depth to nearest meter (determine if there's profiles, discrete depths, or typos)
b <- b %>%
    mutate(r.depth = lapply(depth..m.,
                            function(x)
                                round(x, digits = 0)))
# unique(b[c("station.code","r.depth")]) # Typos= B211.6554, B224.-9999; Discrete = B148 @ 2; Profiles or multiple depths = B211, B143, B22, B224, B266, B317, B409, B430, CROSS
b$r.depth <-as.numeric(as.character(b$r.depth))
    
    # Fixing typo
b$r.depth[b$station.code=="B211"& b$r.depth=="6554"]<- 3 # changing rounded depth to similar depth of other dates

    # Cleaning data frame
b <- b %>% select(!c(time_id,data_id, ORP..mV.,timeperiod)) # Remove unnecessary columns
names(b) <- c("System", "Station", "Date", "Abs.Time", "Abs.Depth","Temp","SC","pH","DO","Tn","Chl","Datetime","Time","Depth") # Rename remaining columns

#cbind(lapply(lapply(b, is.na), sum)) # Identifying columns that have NAs
b1 <- b[!(is.na(b$Temp)) | !(is.na(b$SC)) | !(is.na(b$pH)) | !(is.na(b$DO))| !(is.na(b$Tn))| !(is.na(b$Chl)),] # Removing rows that have no data at all (Some NAs left within single columns for faulty probes, etc.)



b2 <- b1 %>% mutate(
    year = year(Date)
)




s.check<- plyr::ddply(b1, .variables = .(Station), plyr::summarize,
                      t = mean(Temp, na.rm = T),
                      tmin = min(Temp, na.rm = T),
                      tmax = max(Temp, na.rm = T),
                      sc = mean(SC, na.rm = T),
                      scmin = min(SC, na.rm = T),
                      scmax = max(SC, na.rm = T),
                      pH = mean(pH, na.rm = T),
                      pHmin = min(pH, na.rm = T),
                      pHmax = max(pH, na.rm = T),
                      DO = mean(DO, na.rm = T),
                      DOmin = min(DO, na.rm = T),
                      DOmax = max(DO, na.rm = T),
                      Tn = mean(Tn, na.rm = T),
                      Tnmin = min(Tn, na.rm = T),
                      Tnmax = max(Tn, na.rm = T),
                      Chl = mean(Chl, na.rm = T),
                      Chlmin = min(Chl, na.rm = T),
                      Chlmax = max(Chl, na.rm = T))

    # C. Lake Ontario----

    # D. Oneida Lake----   
    
# Save final file as .rdata for easier read into app----