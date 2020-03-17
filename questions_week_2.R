install.packages(tidyverse)
installed.packages()
library(tidyverse)
library(readr)
BOM_data <- read_csv("BOM_data.csv")
BOM_data
View(BOM_data)


BOM_data_temp <- select(BOM_data, Station_number, Temp_min_max, Rainfall, Day) %>% 
  separate(Temp_min_max, into = c("min", "max"), sep='/') %>%  filter(min!='-', max!= '-',
                                                                      Rainfall !='-')
BOM_data_temp

summary <- filtered %>% 
  group_by