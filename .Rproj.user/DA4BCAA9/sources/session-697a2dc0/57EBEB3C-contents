#script for soil texture analysis
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt


#00_Setup_Environment######
#import libraries
library(tidyverse)
library(ggplot2)


#load project functions
source("./03_R/00_functions.R")

#import datasets
ph_clean <- readRDS("./01_input/pH_data_clean.rds")
microbial_c_clean <- readRDS("./01_input/microbialC_data_clean.rds")
microbial_n_clean <- readRDS("./01_input/microbialN_data_clean.rds")


# prepare dataset for plotting#####
#pH
ph_clean_rp <- plot_prepr(ph_clean)

#microbial C
microbial_c_clean_rp <- plot_prepr(microbial_c_clean)

#microbial N
microbial_n_clean_rp <- plot_prepr(microbial_n_clean)


#02 create overview plots#####
location_colors <- map_colours(ph_clean_rp$location) #create colour map vector

#pH overview plot
pH_plot_overview <- overview_plot(ph_clean_rp,sample_name,ph_clean_rp$pH,ph_clean_rp$shape,ph_clean_rp$location,"pH","Field Numbers","pH value",location_colors)
pH_plot_overview  
ggsave("./02_output/pH_overview.png",pH_plot_overview,width = 15,height = 15,units = "cm")


#microbial C overview plot
microbial_c_overview <- overview_plot(microbial_c_clean_rp,microbial_c_clean_rp$sample_name,
                                      microbial_c_clean_rp$microbial_c,microbial_c_clean_rp$shape,
                                      microbial_c_clean_rp$location,"Microbial C","Field Numbers","mg/kg dry mass",location_colors)
microbial_c_overview
ggsave("./02_output/microbial_c_overview.png",microbial_c_overview,width = 15,height = 15,units = "cm")


#microbial N overview plot
microbial_n_overview <- overview_plot(microbial_n_clean_rp,microbial_n_clean_rp$sample_name,
                                      microbial_n_clean_rp$microbial_N,microbial_n_clean_rp$shape,
                                      microbial_n_clean_rp$location,"Microbial N","Field Numbers","mg/kg dry mass",location_colors)

microbial_n_overview
ggsave("./02_output/microbial_n_overview.png",microbial_n_overview,width = 15,height = 15,units = "cm")

