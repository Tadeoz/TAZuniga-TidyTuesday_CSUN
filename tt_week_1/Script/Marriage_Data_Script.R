###Tidy_Tuesday_dubois_challenge_7_dataset
###Created by: Tadeo Aviles Zuniga 
###2022-02-19

###load libraries#####
library(here)
library(tidyverse)

#read in data 
r_download_1<- read_csv("https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge07/data.csv")

#store data in data_raw
data_raw<-r_download_1

#view data
view(data_raw)

#output data into tt_week_1 folder
data_raw %>% 
write_csv(here("tt_week_1","Output","marriage_rates_data.csv"))

data_raw %>% 
  filter(Gender == Gender & Widowed & Group != "Over 65" & Group != "55-65") %>%  # can alter gender to find male & female individually 
ggplot(aes
       (x = Single, # x assigned to single people (predicted value)
        y = Widowed, # (response variable)
         color = Gender))+ #color set to gender 
  scale_colour_viridis_d()+ # add some spicy color 
  geom_jitter()+ # jitter plot , interchangeable geom_point # try new plot next tt
 facet_wrap(~Group)+  #facet data by Groups - easier to distinguish data by age groups
labs(title = "Single Male Widows ", # Adding title 
     subtitle = "Seprated by age groups", # Adding subtitle 
     y = "Widowed", # changing axes titles (x-axis) # y axis label 
     x = "Single", # changing axes titles (y-axis) # x axis label 
     caption = "Tidy Tuesday: Dubois Challenge_7 2022-02-18")+ # Adding caption for the source used for data
  theme(title = element_text(size = 12, color = "blue"))# changing title color

#save plot 
ggsave(here("tt_week_1", "Output", "TT_Single_Widows_Plot.pdf"), # saving plot to week_3 "Outputs" folder 
       width = 7, height = 5) #output margins in inches
  

  
