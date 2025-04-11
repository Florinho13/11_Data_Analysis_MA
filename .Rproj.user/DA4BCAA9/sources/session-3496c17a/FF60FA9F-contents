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
KAK_data <- readRDS("./01_input/KAK_all_2024_09_20.rds")


#01. Prepare Dataset for plotting#####
KAK_data <- KAK_data[2:32] %>% #get rid of sample_key
  mutate_at(vars(4:31),as.numeric)

KAK_clean_rp <- plot_prepr(KAK_data) 

#get rid of values where blanks are measured higher.
KAK_clean_rp <- KAK_clean_rp %>% 
  mutate(across(where(is.numeric), ~ if_else(. < 0, NA_real_, .)))  #set values to NA where the blank values surpass the measured values.



#02. Calculate CEC for whole soil######
KAK_clean_rp_CEC <- KAK_clean_rp %>% 
  mutate("CEC_all" = rowSums(select(., matches("^[a-zA-Z]{2}_aus_kation")), na.rm = TRUE))

#calculate means for the replicates
KAK_replicates_mean <- KAK_clean_rp_CEC %>% 
  filter(!(is.na(replicate))) %>% 
  group_by(sample_name) %>% 
  summarise(across(where(is.numeric),~ mean(.x,na.rm = TRUE)))

write.xlsx(KAK_replicates_mean,"./02_output/KAK_replicates_mean.xlsx")
    








