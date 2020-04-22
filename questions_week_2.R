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

BOM_stations <- read_csv("BOM_stations.csv") 
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
  
  
  arrange(lon)
Long_compare

BOM_data
BOM_data_new
BOM_data_temp
BOM_data_temp_solar
#question1
perth <- filter(BOM_data_temp_solar, Station_number==9225)
perth

perths <- filter(perth, Solar_exposure!="-")
perths
plot1 <-ggplot(data = perths)+
  geom_point(aes(as.numeric(max), as.numeric(min), alpha=0.3), colour="red")+
  xlab("Max Temp")+
  ylab("Min Temp")+
  theme(legend.position = "none")
plot1

#plot2
perths$min<-as.numeric(perths$min)
perths$max<-as.numeric(perths$max)
perths$Rainfall<-as.numeric(perths$Rainfall)
plot2<- ggplot(data = perths, aes(x = max, y = Rainfall) )+
  geom_point(alpha =0.3, colour="blue")
plot2
 perths 

plot3 <-ggplot(data = perths, aes(x = max, y = Solar_exposure))+
  geom_point(alpha = 0.3, colour ="green")
plot3

#question 2
plot4 <-ggplot(data = perths, aes(x = max, y = min))+
  geom_point(aes(size = Rainfall, colour=Solar_exposure))+
  labs(x="Max Temp",
       y="Min Temp",
       size="Solar exposure",
       colour="Rainfall")+
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8))
plot4
#question 3
install.packages("cowplot")
library(cowplot)
theme_set(theme_cowplot())
combined_plot<- plot_grid(plot1, plot2, plot3, plot4, labels = "AUTO")
combined_plot
ggsave(filename = "combined plot.png", plot=combined_plot)

#question 4
BOM_data
BOM_data_new4 <- select(BOM_data, Station_number, Month, Day, Rainfall) %>% 
   filter(Rainfall!='-')
BOM_data_new4$Rainfall <- as.numeric(BOM_data_new4$Rainfall)
BOM_data_new4
Station_mean_rainfall <- group_by(BOM_data_new4, Station_number, Month) %>%
  summarise(Station_mean_rainfall=mean(Rainfall))
Station_mean_rainfall
BOM_Stations_wide$Station_number <- as.numeric(BOM_Stations_wide$Station_number)
BOM_combined2 <- left_join(Station_mean_rainfall, BOM_Stations_wide, by="Station_number")
View(BOM_combined2)

plot5 <-ggplot(data = BOM_combined2, aes(as.factor(Month), Station_mean_rainfall, colour=state, group=Station_number))+
  geom_line(size=1)
plot5

plot6 <-BOM_combined2 %>%
  ggplot(aes(as.factor(Month), Station_mean_rainfall, group=as.factor(Station_number), colour=state))+
  geom_line(size=1)+
  facet_wrap(~state)+
  theme(legend.text = element_text(size = 8), legend.title = element_text(size = 8))+
  labs(x="Month", y="Average rainfall",
       colour="State")
  
plot6
ggsave(filename = "Average rainfall per month for each state.png", plot=plot6)
