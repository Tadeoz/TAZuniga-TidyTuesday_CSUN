###Tidy Tuesday 3
###Aviles Zuniga, Tadeo
###2022-03-30

#load data
library(dplyr)
library(devtools)
library(tidyverse)
library(ggridges)
library(babynames)
library(ggthemes)
library(patchwork)

bn <- babynames[order(-babynames$prop),] %>% filter(complete.cases(.))
bn_F<-bn %>% group_by(sex, year) %>% #filter out everything that is not a complete row 
 filter(year >= 1990 & year <= 2000 & sex == "F")  
view(bn_F)

F1 <- bn_F %>% filter (year == "1991") %>% arrange(-n) 
F2 <- bn_F %>% filter (year == "1992")%>% arrange(-n)
F3 <- bn_F %>% filter (year == "1993")%>% arrange(-n)
F4 <- bn_F %>% filter (year == "1994")%>% arrange(-n)
F5 <- bn_F %>% filter (year == "1995")%>% arrange(-n)
F6 <- bn_F %>% filter (year == "1996")%>% arrange(-n)
F7 <- bn_F %>% filter (year == "1997")%>% arrange(-n)
F8 <- bn_F %>% filter (year == "1998")%>% arrange(-n)
F9 <- bn_F %>% filter (year == "1999")%>% arrange(-n)
F0 <- bn_F %>% filter (year == "2000")%>% arrange(-n)

 F1<-F1[1:5,c(1:5)] %>% drop_na()
 F2<-F2[1:5,c(1:5)] %>% drop_na()
 F3<-F3[1:5,c(1:5)] %>% drop_na()
 F4<-F4[1:5,c(1:5)] %>% drop_na()
 F5<-F5[1:5,c(1:5)] %>% drop_na()
 F6<-F6[1:5,c(1:5)] %>% drop_na()
 F7<-F7[1:5,c(1:5)] %>% drop_na()
 F8<-F8[1:5,c(1:5)] %>% drop_na()
 F9<-F9[1:5,c(1:5)] %>% drop_na()
 F0<-F0[1:5,c(1:5)] %>% drop_na()
 
 x<-full_join(F1,F2)
 y<-full_join(F3,F4)
 z<-full_join(F5,F6)
 i<-full_join(F7,F8)
 j<-full_join(F9,F0)
 h<-full_join(x,y)
 l<-full_join(z,i)
 u<-full_join(j,l)
 p<-full_join(u,h)
 
 p <- p %>% arrange(year)
 view(p)
 
 p %>% 
   ggplot(aes(x = year, 
              y = prop, 
              color = name))+
   geom_point()+
   geom_smooth(method = "lm")+
   geom_jitter()+
   geom_density_ridges(aes(year, y = prop , height = prop, fill = name), 
                                stat = "identity", 
                                scale = 1)+
   labs(title = "Top 5 baby names between 1991-2000",
        y = "Proportion")+
   #guides(color = guide_legend(title = "Coffee Inflation Rates"))+
   theme_dark()+
   theme(
   legend.text = element_text(size = 11),       #title font   
   axis.title.y = element_text(color = "white"),#y axis title font 
   axis.title.x = element_text(color = "white"),#x axis title font
   axis.text.x = element_text(size = 12, #x axis font 
                              color = "white"),
   axis.text.y = element_text(size = 12, #y axis font 
                              color = "white"), #y axis color 
   plot.title = element_text(color = "white", #plot title color 
                             size = 16))+#plot title size 
   scale_fill_viridis_d()
  
#bn_M<-bn_1 %>% group_by(sex, year, name) %>% #filter out everything that is not a complete row 
  #filter(year >= 1900 | year <= 1950 | sex == "M") %>% top_n(100) 


  
  


  
