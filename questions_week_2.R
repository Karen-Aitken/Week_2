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


