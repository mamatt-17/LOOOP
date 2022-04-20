# Script for cleaning and formatting data to be used with LOOOP Shiny page

# Set working directory and load packages----
my_packages <- c("lubridate", "plyr", "dplyr","ggpubr","tidyr", "rLakeAnalyzer")
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
names(buoy.coord) <-c("Station")
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

b3 <- b2 %>% right_join(buoy.coord)

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

# Separate dataframe by Station and fill in time gaps with NAs
station_split <- split(b3,b3$Station)
station_names <- as.character(unique(b3$Station))

for(i in 1:length(station_split)){
  assign(station_names[i],station_split[[i]])
}

B143 <- B143 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day')) %>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B148 <- B148 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B211 <- B211 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B22 <- B22 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B224 <- B224 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B266 <- B266 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B317 <- B317 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B409 <- B409 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
B430 <- B430 %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))
CROSS <- CROSS %>% complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day'))%>% fill(c(Station, lat, long, U.Depths, U.Years)) %>% mutate(year = year(Date))


# Recombine Station dataframes for complete dataframe that can be used to show holes in plots
b4 <- rbind(B143,B148,B211,B22,B224,B266,B317,B409,B430,CROSS)

b4 <- b4 %>% mutate(Abs.Time = replace_na(Abs.Time, "00:00:00")) %>% mutate(Datetime = as.POSIXct(paste(Date, Abs.Time), format = "%Y-%m-%d %H:%M"))

b5 <- b4 %>% pivot_longer(.,c(Temp,SC,pH,DO,Tn,Chl), names_to = "params",values_to = "value")

b5$Depth <- as.factor(as.character(b5$Depth))
b5$params <- as.factor(as.character(b5$params))


# TESTING PLOT CODE FOR APP
b6 <- b5 %>% filter(Station == "B211",params=="Temp",Depth == "2") %>% 
  complete(Date = seq.Date(min(Date, na.rm = T), max(Date, na.rm = T), by = 'day')) %>% 
  fill(c(Station, Depth, params ,lat, long, U.Depths, U.Years))%>% 
  mutate(Abs.Time = replace_na(Abs.Time, "00:00:00"),
         Datetime = as.POSIXct(paste(Date, Abs.Time), format = "%Y-%m-%d %H:%M" ))

ggplot(data = b6, mapping = aes(x = Datetime, y = value))+
  geom_point(size = 2)+
  geom_line()+
  theme_minimal()+
scale_y_continuous(name =  "Temperature (deg. C)",
                   limits = c(floor(min(b6$value, na.rm = T)),ceiling(max(b6$value,na.rm = T))),
                   breaks = c(seq(floor(min(b6$value, na.rm = T)),ceiling(max(b6$value,na.rm = T))),.5))+
scale_x_datetime(name = "Date",
                 date_breaks = "1 month",
                 date_labels = "%b %y",
                 date_minor_breaks = "1 day")+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(size = 12)
  )


# C. Lake Ontario----

    # D. Oneida Lake----   
    
# Save final file as .rdata for easier read into app----

save(b5, file = "looop.rdata")
