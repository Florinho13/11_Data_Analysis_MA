#script for CNS analysis
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt


#00_Setup_Environment######
#import libraries
library(tidyverse)
library(ggplot2)


#load project functions
source("./03_R/00_functions.R")

#import datasets
CNS_data <- readRDS("./01_input/CNS_data_clean.rds")


#01. Calculate Corg, and averages of the replicates ############
#get dataframe for normal samples
CNS_data_normal <- CNS_data %>% 
  filter(C_fraction == "normal") %>% 
  select(sample_name,replicate,'N%','C%', '%S','C/N')

#get dataframe for muffled samples
CNS_data_muffled <- CNS_data %>% 
  filter(C_fraction == "muffled") %>% 
  select(sample_name,replicate,'N%','C%', '%S','C/N')


#join normal and muffled dataset
CNS_data_normal_muffled <- CNS_data_normal %>% 
  left_join(CNS_data_muffled,by = c("sample_name","replicate"),suffix = c("_normal","_muffled")) %>% 
  mutate_at(vars(3:10),as.numeric)

#calculate mean values within the replicates of each plot
CNS_data_normal_muffled_mean <- CNS_data_normal_muffled %>% 
  group_by(sample_name) %>% 
  summarise(across(where(is.numeric),~ mean(.x)))

#calculate Corg
#preparation step replace NA values in the needed columns with 0
CNS_data_normal_muffled_mean <- CNS_data_normal_muffled_mean %>%
  mutate(`C%_normal` = replace(`C%_normal`, is.na(`C%_normal`), 0),
         `C%_muffled` = replace( `C%_muffled`, is.na( `C%_muffled`), 0))

#Calculate columns for Corg and Humus content
CNS_data_normal_muffled_mean_Corg <- CNS_data_normal_muffled_mean %>% 
  mutate(Corg = `C%_normal` - `C%_muffled`) %>% 
  mutate(Humus = Corg * 1.72) #factor 1.72 is based on a FIBL publication from 2021 "Bodenuntersuchungen für Biobetriebe

write.xlsx(CNS_data_normal_muffled_mean_Corg,"./02_output/CNS_measurement_mean_Corg_humus.xlsx")

#02. prepare for plotting
CNS_clean_mean_Corg_rp <- plot_prepr(CNS_data_normal_muffled_mean_Corg)
