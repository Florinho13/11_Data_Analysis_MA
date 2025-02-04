#script for bulk density analysis
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt


#00_Setup_Environment######
#import libraries
library(tidyverse)
library(ggplot2)


#load project functions
source("./03_R/00_functions.R")


#import datasets
bulk_density <- readRDS("./01_input/bulk_density_clean.rds")

#01. calculate means per field #####
bulk_density_mean <- bulk_density %>% 
  group_by(sample_name) %>% 
  summarise(mean(BD_g_cm3)) %>% 
  rename()

write.xlsx(bulk_density_mean,"./02_output/bulk_density_mean.xlsx")
