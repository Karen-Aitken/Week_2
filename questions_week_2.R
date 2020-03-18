install.packages(tidyverse)
installed.packages()
library(tidyverse)
library(readr)
BOM_data <- read_csv("BOM_data.csv")
BOM_data
View(BOM_data)
#Quetion 1 For each station how many days have a minimum temp, max temp, and a rainfall measurement

BOM_data_temp <- select(BOM_data, Station_number, Temp_min_max, Rainfall, Day) %>% 
  separate(Temp_min_max, into = c("min", "max"), sep='/') %>%  filter(min!='-', max!= '-',
                                                                      Rainfall !='-')
BOM_data_temp

result <- BOM_data_temp %>% 
  group_by(Station_number) %>% 
  summarise(n_days = n())
# Question 2 which month saw the lowest average daily tem diff

BOM_data_new <- select(BOM_data, Station_number, Month, Day, Temp_min_max) %>% 
  separate(Temp_min_max, into = c("min", "max"), sep='/') %>%  filter(min!='-', max!= '-')
BOM_data_new$min <- as.numeric(BOM_data_new$min)
BOM_data_new$max <- as.numeric(BOM_data_new$max)
mutate(BOM_data_new, dif_temp = max- min) %>% group_by(Month) %>%
  summarise(n_dif_temp=mean(dif_temp))


# question 3

BOM_stations <- read_csv("BOM-stations.csv") 
BOM_stations
View(BOM_stations)
?gather
# gather the data into 3 columns  station ID number , info 
BOM_stations_long  <- 
  gather(BOM_stations, key = "Station_number", "data", -info)
View(BOM_stations_long)
#spread the data one row for each station

BOM_Stations_wide <- spread(BOM_stations_long, key="info", value="data")
View(BOM_Stations_wide)

BOM_data_temp$Station_number <- as.numeric(BOM_data_temp$Station_number)
BOM_Stations_wide$Station_number <- as.numeric(BOM_Stations_wide$Station_number)
 
#BOM_group <- group_by(BOM_data_temp, Station_number)
#View(BOM_group)

BOM_combined <- left_join(BOM_data_temp, BOM_Stations_wide, by="Station_number")
View(BOM_combined)

unique(BOM_combined$min) 

BOM_mean_combined <- BOM_combined %>%
  mutate(min_num =as.numeric(min),max_num =as.numeric(max)) %>% 
group_by(state) %>% 
  mutate(temp_diff= max_num-min_num) %>% 
  filter(!is.na(temp_diff)) %>% 
summarise(mean_temp=mean(temp_diff)) %>% 
  arrange(mean_temp)
View(BOM_mean_combined)
 
#question 4 Does the lowest longitude or highest lontitude weather station in our dataset have a higher average solar exposure?

BOM_data <- read_csv("BOM_data.csv")
BOM_data
View(BOM_data)
#Question 1 For each station how many days have a minimum temp, max temp, and a rainfall measurement

BOM_data_temp_solar <- select(BOM_data, Station_number, Temp_min_max, Rainfall, Day, Solar_exposure) %>% 
  separate(Temp_min_max, into = c("min", "max"), sep='/') %>%  filter(min!='-', max!= '-',
                                                                      Rainfall !='-', Solar_exposure !='-')
BOM_data_temp_solar

BOM_data_temp_solar$Solar_exposure<-as.numeric(BOM_data_temp_solar$Solar_exposure)
BOM_Stations_wide$lon <- as.numeric(BOM_Stations_wide$lon)
BOM_Stations_wide
#BOM_data_temp_solar
Station_mean_solar <- group_by(BOM_data_temp_solar, Station_number) %>% 
  summarise(mean_solar_exposure=mean(Solar_exposure))
Station_mean_solar
Long_compare <- inner_join(Station_mean_solar, BOM_Stations_wide, by="Station_number") %>% 
filter(lon==min(lon) |lon==max(lon))
  #summarise(mean_solar_lon=mean(mean_solar_exposure)) 
  
  arrange(lon)
Long_compare



