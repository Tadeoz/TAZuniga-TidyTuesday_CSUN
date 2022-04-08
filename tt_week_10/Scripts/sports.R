
            # Install gridExtra package
library("gridExtra")    
library(reshape)
library(tidyverse)
library(dplyr)
library(here)
library(devtools)
library(ggplot2)
library(ggthemes)
library(gapminder)
install.packages("scico")
library(scico)
library(paletteer)



sports <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-29/sports.csv')

view(sports[1:30, c(1:28)])
summary(sports)
ncol(sports)
nrow(sports)


head(bball)
view(bball)
############### Total Revenue ###############3
bball_t <- sports %>% 
 filter(sports == "Basketball") %>% 
  arrange(-total_rev_menwomen) %>% 
  slice_head(n = 500) %>% 
  select(year,institution_name, total_rev_menwomen, rev_men, rev_women, state_cd) %>% 
  drop_na()


total_revenue <-bball_t[c(1:100),] %>% 
ggplot(aes(x = year , y = institution_name, fill = total_rev_menwomen)) +
  geom_tile()+
  labs(x = " Year", 
       y = "Institution",
       title = "Total Revenue in Men and Women's Basketball",
       legend = "Total Revenue")+
  guides(fill = guide_legend(title = "Total Revenue"))+
  theme_dark()+
  theme(panel.background = element_rect(fill = "black", color = "grey"),
        legend.text = element_text(size = 11),       #title font   
        axis.title.y = element_text(color = "black"),#y axis title font 
        axis.title.x = element_text(color = "black"),#x axis title font
        axis.text.x = element_text(size = 10, #x axis font 
                                   color = "black"),
        axis.text.y = element_text(size = 5.5, #y axis font 
                                   color = "black"), #y axis color 
        plot.title = element_text(color = "black", #plot title color 
                                  size = 16))+#plot title size )+
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805",
                      guide = "colorbar")
print(total_revenue)

############### Women's  Revenue ###############
bball_w <- sports %>% 
  filter(sports == "Basketball") %>% 
  arrange(-rev_women) %>% 
  slice_head(n = 500) %>% 
  select(year,institution_name, total_rev_menwomen, rev_men, rev_women, state_cd) %>% 
  drop_na()

view(bball_w)

women_rev <- bball_w[c(1:100),] %>% 
  ggplot(aes(x = year , y = institution_name, fill = rev_women)) +
  geom_tile()+
  labs(x = " Year", 
       y = "Institution",
       title = "Total Revenue in Women's Basketball",
       legend = "Total Revenue")+
  guides(fill = guide_legend(title = "Women's Revenue"))+
  theme_dark()+
  theme(panel.background = element_rect(fill = "black", color = "grey"),
        legend.text = element_text(size = 11),       #title font   
        axis.title.y = element_text(color = "black"),#y axis title font 
        axis.title.x = element_text(color = "black"),#x axis title font
        axis.text.x = element_text(size = 10, #x axis font 
                                   color = "black"),
        axis.text.y = element_text(size = 5.5, #y axis font 
                                   color = "black"), #y axis color 
        plot.title = element_text(color = "black", #plot title color 
                                  size = 16))+#plot title size )+
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805",
                      guide = "colorbar")
print(women_rev)


############### Men's   Revenue ###############
bball_m <- sports %>% 
  filter(sports == "Basketball") %>% 
  arrange(-rev_men) %>% 
  slice_head(n = 500) %>% 
  select(year,institution_name, total_rev_menwomen, rev_men, rev_women, state_cd) %>% 
  drop_na()

men_rev <- bball_m[c(1:100),] %>% 
  ggplot(aes(x = year , y = institution_name, fill = rev_men)) +
  geom_tile()+
  labs(x = " Year", 
       y = "Institution",
       title = "Total Revenue in Men's Basketball",
       legend = "Total Revenue")+
  guides(fill = guide_legend(title = "Men's Revenue"))+
  theme_dark()+
  theme(panel.background = element_rect(fill = "black", color = "grey"),
        legend.text = element_text(size = 11),       #title font   
        axis.title.y = element_text(color = "black"),#y axis title font 
        axis.title.x = element_text(color = "black"),#x axis title font
        axis.text.x = element_text(size = 10, #x axis font 
                                   color = "black"),
        axis.text.y = element_text(size = 5.5, #y axis font 
                                   color = "black"), #y axis color 
        plot.title = element_text(color = "black", #plot title color 
                                  size = 16))+#plot title size )+
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805",
                      guide = "colorbar")
print(men_rev)


grid.arrange(total_revenue, men_rev, women_rev, nrow = 3) 


socc_t <- sports %>% 
  filter(sports == "Soccer") %>% 
  arrange(-rev_women) %>% 
  slice_head(n = 500) %>% 
  select(year,institution_name, total_rev_menwomen, rev_men, rev_women, state_cd) %>% 
  drop_na()

  
socc_t[c(1:100),] %>% 
  ggplot(aes(x = year , y = institution_name, fill = rev_men)) +
  geom_tile()+
  labs(x = " Year", 
       y = "Institution",
       title = "Total Revenue in Men and Women's Soccer",
       legend = "Total Revenue")+
  guides(fill = guide_legend(title = "Total Revenue"))+
  theme_dark()+
  theme(panel.border = element_rect(color = "#353436",
                                    fill = NA,
                                    size = 6),
    panel.background = element_rect(fill = "black", color = "grey"),
        legend.text = element_text(size = 11),       #title font   
        axis.title.y = element_text(color = "black"),#y axis title font 
        axis.title.x = element_text(color = "black"),#x axis title font
        axis.text.x = element_text(size = 10, #x axis font 
                                   color = "black"),
        axis.text.y = element_text(size = 5.5, #y axis font 
                                   color = "black"), #y axis color 
        plot.title = element_text(color = "black", #plot title color 
                                  size = 16))+#plot title size )+
        paletteer::scale_fill_paletteer_c("gameofthrones::targaryen")
  
  
  scico::scale_fill_scico(palette = "lajolla")
  
  
  scale_fill_gradient(low = "#353436",
                      high = "#f6f805",
                      guide = "colorbar")
print(men_rev)
