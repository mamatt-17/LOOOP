# Script for cleaning and formatting data to be used with LOOOP Shiny page

# Set working directory and load packages----
my_packages <- c("lubridate", "plyr", "dplyr","tidyr", "stringi")
lapply(my_packages, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in csv files of each dataset----
# 3 Rivers data, read all files into 1 dataset----
filenames <- list.files(path = './CSV_files/3Rivers', pattern = '*', full.names = TRUE)
b <- ldply(filenames, read.csv)

# Weather Station data, read all files into 1 dataset----
filenames <- list.files(path = './CSV_files/WeatherStations', pattern = '*', full.names = TRUE)
w <- ldply(filenames, read.csv)
# Stream Survey data, read in CSV----
s <- read.csv(file = './CSV_files/StreamData.csv')
# Stream level data, read all files into 1 dataset----
filenames <- list.files(path = './CSV_files/StreamLevels', pattern = '*', full.names = TRUE)
l <- ldply(filenames, read.csv)
#----
# 3 RIVERS----
# Manipulated 3 Rivers data so that it meets requirements for visualizations----
b$system.code <- as.character(b$system.code)
b$date <- as.Date(b$date,format = "%m/%d/%Y")
b$datetime <- as.POSIXct(paste(b$date, b$time), format = "%Y-%m-%d %H:%M") # Time cannot exist without date

# Adding time period factor based on collection time (1. 21:00-3:00, 2.  3:01-9:00, 3. 9:01-15:00, 4. 15:01-21:00)----
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

# Factor stations and add coordinates----
b$station.code <- as.factor(as.character(b$station.code))
buoy.coord <-as.data.frame(unique(b$station.code))
names(buoy.coord) <-c("Station")
buoy.coord$lat <-c(43.240093,43.44995,43.205123,43.231178,43.193388,43.139376,43.147516,43.103883,43.100510,43.108055)
buoy.coord$long <- c(-76.147568,-76.50349,-76.269799,-76.309791,-76.279911,-76.238025,-76.314771,-76.445725,-76.499537,-76.475456)
#unique(b[c("system.code","station.code")],) # Check that the buoys are in the right river

# Round depth to nearest meter (determine if there's profiles, discrete depths, or typos)----
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

b3 <- b2 %>% right_join(buoy.coord)

# Unique dataframes----
u.b <- data.frame(unique(b3[c("Station","Depth","year")]))

u.b <- u.b[order(u.b$Depth),]

u.b <- u.b %>% group_by(Station) %>% 
  summarise_all(funs(trimws(paste(., collapse = ", "))))

u.b2 <- as.data.frame(unique(u.b$Station))
u.b2$U.Depths <- c("2-5 meters","2 meters","2-3 meters","1-3 meters","1-4 meters","1-6 meters","1-7 meters","1-6 meters","1-4 meters","1-16 meters")
u.b2$U.Years <- c("2008, 2009",
                  "2008, 2009",
                  "2008, 2009",
                  "2008, 2009",
                  "2002, 2003, 2008, 2009",
                  "2000 , 2006",
                  "2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009",
                  "2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009",
                  "2004, 2005, 2006, 2007, 2008, 2009",
                  "2006, 2009")
names(u.b2) <- c("Station","U.Depths","U.Years")
b3 <- b3 %>% right_join(u.b2) %>% select(!c(System))

# Separate dataframe by Station and fill in time gaps with NAs----
station_split <- split(b3,b3$Station)
station_names <- as.character(unique(b3$Station))

for(i in 1:length(station_split)){
  assign(station_names[i],station_split[[i]])
}

B143 <- B143 %>% arrange(Date) %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day')) %>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B148 <- B148 %>% arrange(Date) %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B211 <- B211 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B22 <- B22 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B224 <- B224 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B266 <- B266 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B317 <- B317 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B409 <- B409 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B430 <- B430 %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
CROSS <- CROSS %>% arrange(Date) %>%complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))


# Recombine Station dataframes for complete dataframe that can be used to show holes in plots----
b4 <- rbind(B143,B148,B211,B22,B224,B266,B317,B409,B430,CROSS)

b4 <- b4 %>% mutate(Abs.Time = replace_na(Abs.Time, "00:00:00")) %>% mutate(Datetime = as.POSIXct(paste(Date, Abs.Time), format = "%Y-%m-%d %H:%M"))

b5 <- b4 %>% pivot_longer(.,c(Temp,SC,pH,DO,Tn,Chl), names_to = "params",values_to = "value")

b5$Depth <- as.factor(as.character(b5$Depth))
b5$params <- as.factor(as.character(b5$params))

b5 <- b5 %>% mutate(Ylabel = case_when(
  stri_detect_regex(params, "Temp") ~ "Temperature (deg.C)",
  stri_detect_regex(params, "SC") ~ "Specific Conductance (uS/cm)",
  stri_detect_regex(params, "pH") ~ "pH (units)",
  stri_detect_regex(params, "DO") ~ "Dissolved Oxygen (mg/l)",
  stri_detect_regex(params, "Tn") ~ "Turbidity (NTU)",
  stri_detect_regex(params, "Chl") ~ "Chlorophyll-a (ug/l)"
))


#b5 <- b5 %>% group_by(Station)%>% arrange(Depth, .by_group = TRUE)

#list.dataframe <- list(B143,B148,B211,B22,B224,B266,B317,B409,B430,CROSS)

river.data <- b5 %>% arrange(Date)

# Saving a smaller dataframe for the map----
mapframe <- as.data.frame(unique(b5[c("Station","U.Depths","lat","long","U.Years")]))


# ----
# WEATHER STATION DATA----
# Restructure dataframe----
w$Group <- as.factor(as.character(w$Group))
w$Site.Name <-as.factor(as.character(w$Site.Name))
w$Date <- as.Date(w$Date,format = "%m/%d/%Y")
w$WindDirection <- as.factor(as.character(w$WindDirection))

# Round Times to the nearest quarter hour and make new dataframe with just hourly data for app----
w$Datetime <- as.POSIXct(paste(w$Date, w$Time), format = "%Y-%m-%d %H:%M:%S")
w$Datetime <- round_date(w$Datetime, "15 minutes")
w <- w %>% select(!c(Date, Time))
# Rename Parameter columns----
w <- w %>% rename(., 'Rain (mm)' = Rain.mm, 'Temperature (deg. C)' = Temp, 'Relative Humidity (%)' = RH,
                  'Wind Speed (m/s)' = WindSpeed.ms, 'Gust Speed (m/s)' = GustSpeed.ms,
                  'Wind Direction (theta)' = WindDirection.theta, 'Wind Direction' = WindDirection)
# Reshape to long format with a parameters column----
w.long <- w %>% pivot_longer(!c(Group, Site.Name, Latitude, Longitude, Datetime, 'Wind Direction'), 
                             names_to = "params", values_to = "Result")
# Create hourly dataframe for app----
w.long <- w.long %>% group_by(Datetime = round_date(Datetime, 'hour'), Group, Site.Name, Latitude, Longitude, params) %>% 
  summarise(Result = round(mean(Result, na.rm = T), 2)) %>% ungroup()

# Pull out hourly wind theta to recalculate average wind direction and append to dataframe----
windtheta <- w.long %>% filter(params == "Wind Direction (theta)")
windtheta <- windtheta %>% mutate(winddir = round(1+(Result)/45, 0)) %>% 
  mutate('Wind Direction' = lapply(winddir,
                                   function(y)
                                     if (y == 1)
                                       "North" else
                                         if (y == 2)
                                           "North-East" else
                                             if (y == 3)
                                               "East" else
                                                 if (y == 4)
                                                   "South-East" else
                                                     if (y == 5)
                                                       "South" else
                                                         if (y == 6)
                                                           "South-West" else
                                                             if (y == 7)
                                                               "West" else
                                                                 if (y == 8)
                                                                   "North-West" else
                                                                     if (y == 9)
                                                                       "North"
                                   )) %>% select(c(Datetime, Group, Site.Name, Latitude, Longitude, 'Wind Direction'))

# Add Summarized Wind Direction back to hourly dataframe
w.long <- w.long %>% left_join(windtheta) %>% relocate('Wind Direction', .after = Longitude)

rm(windtheta)

#----
# STREAM SURVEY DATA----
# Restructure dataframe----
s$Group <- as.factor(as.character(s$Group))
s$Site.Name <- as.factor(as.character(s$Site.Name))
s$Location <- as.factor(as.character(s$Location))
s$Date <- as.Date(s$Date,format = "%m/%d/%Y")
s$Datetime <- as.POSIXct(paste(s$Date, s$Time), format = "%Y-%m-%d %H:%M")
s$Datetime <- round_date(s$Datetime, "1 hour")
s <- s %>% select(!c(Date, Time)) %>% relocate(Datetime, .after = Longitude)
s$Water.Temperature <- as.numeric(as.character(s$Water.Temperature))
s$Air.Temperature <-as.numeric(as.character(s$Air.Temperature))
s$Nitrite <- as.numeric(as.character(s$Nitrite))
s$Nitrate <- as.numeric(as.character(s$Nitrate))
s$Alkalinity <- as.numeric(as.character(s$Alkalinity))
s$pH <- as.numeric(as.character(s$pH))
s$Dissolved.Oxygen <- as.numeric(as.character(s$Dissolved.Oxygen))
s$Chloride <- as.numeric(as.character(s$Chloride))
s$Turbidity <- as.numeric(as.character(s$Turbidity))
s$Biological.Index <-as.numeric(as.character(s$Biological.Index))


# Rename Parameter columns----
s <- s %>% rename(., 'Water Temperature (deg. C)' = Water.Temperature, 'Air Temperature (deg. C)' = Air.Temperature, 'Stream Width (ft)' = Stream.Width,
                  'Average Stream Depth (ft)' = Avg.Stream.Depth, 'Stream Area (square ft)' = Stream.Area, 'Average Stream Velocity (ft/s)' = Avg.Stream.Velocity,
                  'Streamflow (cubic ft/s)' = Streamflow, 'Nitrite (mg/L)' = Nitrite, 'Nitrate (mg/L)' = Nitrate, 'Alkalinity (mg/L)' = Alkalinity, 'pH (units)' = pH,
                  'Dissolved Oxygen (mg/L)' = Dissolved.Oxygen, 'Phosphate (mg/L)' = Phosphate, 'Chloride (mg/L)' = Chloride, 'Turbidity (cm)' = Turbidity,
                  'Macroinvertebrates Present' = Macroinvertebrates, 'Biological Index Score' = Biological.Index)
# Reshape to long format with a parameters column----
s.long <- s %>% pivot_longer(!c(Group, Site.Name, Location ,Latitude, Longitude, Datetime, 'Macroinvertebrates Present'),
                             names_to = "params", values_to = "Result")


#----
# STREAM LEVEL DATA----
#----
# Save final file as .rdata for easier read into app----

#save(b5, mapframe, file = "looop.rdata")
#write.csv(river.data, file = "river_data.csv")
#save(river.data, file = "riverdata.rdata")
#save(mapframe, file = "mapframe.rdata")
save(s.long, w.long, file = "student.rdata")
