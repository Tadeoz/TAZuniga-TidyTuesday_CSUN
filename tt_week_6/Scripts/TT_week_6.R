### Tidy_Tuesday Project 2: Data from Los Angeles fuel source type for utility purpose throughout the various regions in the city 
### Aviles Zuniga,Tadeo 
### 2022-03-07

#install.packages("ggthemes")
#install.packages("gplots")
#install.packages("ggplot2")

#load libraries 
library(tidyverse)
library(here)
library(ggplot2)
library(gplots)
library(ggthemes)


#load data
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')
view(stations)

#filter data for utility companies in the los angeles area
stations_<-  stations %>% 
  select(STATION_NAME, CITY, STATE, LONGITUDE, LATITUDE, FACILITY_TYPE, FUEL_TYPE_CODE) %>% 
  filter(STATE == "CA" & FACILITY_TYPE != "UTILITY" & (CITY == "Woodland HIlls" | CITY == "Van Nuys" |  CITY == "Sherman Oaks" |  CITY == "Los Angeles" |  CITY == "Northridge")) %>% 
  group_by(FUEL_TYPE_CODE) #group by fuel type 
  
#stations_ %>% 
 # write_csv(here("tt_week_6","Output","stations_Los_Angeles.csv"))
#head(station_)
#view(stations_)

stations_ %>% drop_na(CITY) %>% #drop NA from cities
      ggplot(aes(x = FUEL_TYPE_CODE, # set x to fuel type 
                 y = FACILITY_TYPE, # set y to facility type 
                 color = FUEL_TYPE_CODE, # set color to fuel type 
                 alpha = .2,
                 size = 1))+
  geom_jitter(size = 1, alpha = 0.2)+
  geom_smooth()+
  labs(title = "Los Angeles Fuel types by Facility Type",#labels title, x axis and y axis 
       x = " Fuel Type",
       y = "Facility Type")+ 
  facet_grid(~CITY)+#facet by city 
  
  theme_solarized(light = FALSE)+#theme solarized, causes cool background 
  scale_color_manual(values = c("red1","pink3","white", "pink3", "white","red2"))+#manually set colors to red white and pink
  guides(color = guide_legend(title = "Fuel Types"))+#label legend title 
  theme(
    legend.text = element_text(size = 9),       #title font   
    axis.title.y = element_text(color = "red"),#y axis title font 
    axis.title.x = element_text(color = "red"),#x axis title font
    axis.text.x = element_text(size = 4.5, #x axis font 
                                   color = "white"),
    axis.text.y = element_text(size = 5.5, #y axis font 
                               color = "white"), #y axis color 
    plot.title = element_text(color = "red", #plot title color 
                                  size = 15)) #plot title size 
ggsave(here("tt_week_6","Output", "TT_LosAngeles_Fuel_plot.png"),#save plot in "Output" folder
       width = 7, height = 5)
 

#####################################################################################################3
#US_MAP code for future refrence

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

plot_usmap(include = c("CA", "ID", "NV", "OR", "WA")) +
  labs(title = "Western US States",
       subtitle = "These are the states in the Pacific Timezone.")

list(scale_color_manual())
