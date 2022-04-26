 library(devtools)
 library(devtools)
 library(ggplot2)
 library(usethis)
 library(tidyverse)
 library(ComplexHeatmap)
 library(RColorBrewer)
 library(circlize)


ip <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/indoor_pollution.csv')
view(ip)
 

j<-ip %>% mutate(Deaths =`Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)`) %>%
  select(Entity, Year, Deaths) %>% group_by(Entity)  %>% 
 arrange(-Deaths) %>% top_n(1) %>% head(n=10) %>% 
  as.matrix

Heatmap(j [,c("Year", "Deaths")], name = "Pollution Matrix" , 
        row_title = "Death by Pollution",
        cluster_rows = TRUE,
        clustering_distance_rows = "euclidean",
        clustering_method_rows = "complete",
        row_dend_side = c("left", "right"),
        column_title = "Matrix of Pollution related Deaths",
        show_heatmap_legend = TRUE,
        show_row_names = TRUE,
        show_column_names = FALSE,
        #show_column_dend = TRUE,
        heatmap_legend_param = list(title = "Pollution Matrix",
                                    labels_gp = gpar(fontsize = 7),
                                    title_position = c("topleft"),
                                    color_bar = c("discrete"),
                                    labels_gp = gpar(fontsize = 7),
                                    title_gp = gpar(fontsize = 10, fontface = "bold"),
                                    nrow = NULL, 
                                    ncol = 3))
                                   
    
color_mapping_legend(object, ...,
                    plot = TRUE,
                    title = object@name,
                    title_gp = gpar(fontsize = 10, fontface = "bold"),
                    title_position = c("topleft", "topcenter", "leftcenter", "lefttop"),
                    color_bar = c("discrete", "continuous"),
                    grid_height = unit(4, "mm"),
                    grid_width = unit(4, "mm"),
                    grid_border = NULL,
                    at = object@levels,
                    labels = at,
                    labels_gp = gpar(fontsize = 10),
                    nrow = NULL,
                    ncol = 1,
                    legend_height = NULL, legend_width = NULL,
                    legend_direction = c("vertical", "horizontal"),
                    param = NULL)

