#script for basic plant health data analysis
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt

#00_setup_environment#####
#import libraries
library(tidyverse)
library(ggplot2)

#source functions
source("./03_R/00_functions.R")

#import datasets
#basic chlorophyll data
chlorophyll_data_clean <- readRDS("./01_input/plant_chlorophyll_data_clean.rds")

#long chlorophyll data
chlorophyll_data_long_clean <- readRDS("./01_input/plant_chlorophyll_data_clean_long.rds")

#plant height data
height_data_clean <- readRDS("./01_input/plant_height_data_clean.rds")


#01 prepare datasets for plotting#####
#basic chlorophyll data
chlorophyll_data_clean_rp <- plot_prepr(chlorophyll_data_clean)

#long chlorophyll data
chlorophyll_data_long_clean_rp <- plot_prepr(chlorophyll_data_long_clean)

#plant height
plant_height_clean_rp <- plot_prepr(height_data_clean)


#03 create overview plots
location_colors <- map_colours(chlorophyll_data_clean_rp$location) #create colour map vector


#simple chlorophyll data (1*mid/1*front)
chlorophyll_front_plot_overview <- overview_plot(chlorophyll_data_clean_rp,chlorophyll_data_clean_rp$sample_name,
                                                 chlorophyll_data_clean_rp$chlorophyll_front,chlorophyll_data_clean_rp$shape,
                                                 chlorophyll_data_clean_rp$location,"chlorophyll content leaves front",
                                                 "Field Values", "SPAD Values",location_colors)
chlorophyll_front_plot_overview

chlorophyll_boxplot <- ggplot(chlorophyll_data_long_clean_rp,aes(x=substr(sample_name,1,3),y=chlorophyll_value,fill = location,colour = chlorophyll_type))+
  geom_boxplot()+
  ggtitle("Chlorophyll Content of Oilseed Rape leaves per field")+
  labs(x="Field Numbers",y="SPAD values")+
  stat_summary(fun = mean, geom = "point", aes(shape = factor(farming_system)), size = 3.5, colour = "black") +
  scale_shape_manual(values = chlorophyll_data_long_clean_rp$shape, labels = c("Regenerative", "Conventional")) +
  scale_fill_manual(values = location_colors,labels = c("Freudwil","Niederhasli","Läufelfingen",
                                                               "Heimenhausen","Heimiswil","Ueberstorf"))+
  scale_colour_manual(values = c("lightgreen","lightblue"),labels = c("Chlorophyll Front", "Chlorophyll Mid"))+
  labs(fill = "Location",shape="Farming System", colour = "Measurement Point")+
  theme_minimal()
chlorophyll_boxplot
ggsave("./02_output/chlorophyll_boxplot.png",chlorophyll_boxplot,width = 15,height = 15,units = "cm")


#plant height
height_plot_overview <- overview_plot(plant_height_clean_rp,plant_height_clean_rp$sample_name,
                                      plant_height_clean_rp$height_cm,plant_height_clean_rp$shape,
                                      plant_height_clean_rp$location,"plant height",
                                      "Field Values", "Height [cm]",location_colors)
  #facet_wrap(~plot)
height_plot_overview
ggsave("./02_output/plant_height_plot.png",height_plot_overview,width = 15,height = 15,units = "cm")

