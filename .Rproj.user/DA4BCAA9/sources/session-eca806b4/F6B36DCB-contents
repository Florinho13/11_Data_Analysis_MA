#script to get evaluation of ppp measurements in soil
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt

#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggttext)
library(hrbrthemes)
library(patchwork)
library(flextable)
library(officer)



#load project functions
source("./03_R/00_functions.R")


#load data
ppp_all_cleaned_long_meta_rq <- readRDS("./02_output/11_ppp_comparison/ppp_all_cleaned_long_meta_rq.rds")
sprint_ppp_soil <- read.xlsx("./02_output/11_ppp_comparison/SPRINT_ppp_soil.xlsx",sheet = 1)
ppp_metadata_sprint_coding <- read.xlsx("./01_input/Metadata_All_CSS_EHA_and_PPP_coding_Aug_2023.xlsx",sheet = 5)
pnec_soil_data <- read.xlsx("./01_input/PNEC_soil_water_list_120722.xlsx", sheet = 2)[,c(1,24)]
ppp_info_set <- read.xlsx("./01_input/PPP_Info_D2_3.xlsx")


#01.1 prepare data for comparison plotting#####
#bring sprint data to long format
sprint_ppp_soil_long <- sprint_ppp_soil[1:213] %>% 
  pivot_longer(cols = 6:213,
               names_to = "ppp_compound",
               values_to = "concentrations_ng_g") %>% 
  mutate(detected = if_else(concentrations_ng_g > 0, 1, 0),.after = 7)

#align with ppp data thesis
sprint_ppp_soil_long_meta <- sprint_ppp_soil_long %>% 
  left_join(ppp_info_set_clean,by = join_by("ppp_compound" == "PPP_compound_clean_name")) %>% 
  select(-"PPP.compound") %>% 
  filter(ppp_compound != "Imidacloprid",
         ppp_compound != "Metalaxyl_Metabolite") %>% 
  rename("sample" = "sprint_sample_code",
         "farming_system" = "farmtype")

#01.2 get total_substances for both datasets

sprint_ppp_soil_long_meta_freq_plot <- sprint_ppp_soil_long_meta %>% 
  group_by(sample, farming_system) %>% 
  summarise(n_detected = sum(detected,na.rm = TRUE), .groups = "drop") %>% 
  mutate(farming_system = str_replace(farming_system, "Conventional","2"),
         farming_system = str_replace(farming_system, "Organic","3")) %>% 
  mutate(dataset = substr(sample,1,2)) %>% 
  filter(dataset %in% c("CZ","CH"))

thesis_ppp_soil_long_meta_freq_plot <- ppp_all_cleaned_long_meta_rq %>% 
  group_by(sample,farming_system) %>% 
  summarise(n_detected = sum(detected,na.rm = TRUE), .groups = "drop") %>% 
  mutate(dataset = "Thesis")

sprint_thesis_combined_freq_plot <- thesis_ppp_soil_long_meta_freq_plot %>% 
  bind_rows(sprint_ppp_soil_long_meta_freq_plot)


ggplot(data = sprint_thesis_combined_freq_plot,
       aes(x = dataset,y = n_detected, fill = farming_system))+
  geom_boxplot() +
  ggtitle("Comparison Boxplot of Different Datasets and Farming Systems")+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    linetype = "dashed",
    width = 0.5,        # diamond shape
    flatten = 0,
    color = "red",
    fill = "white"
  )+
  scale_fill_manual(values = fs_colour_plus, 
                     labels = c("Regenerative", "Conventional", "Organic"))+
  scale_x_discrete(labels = c(
    "Thesis" = "Thesis, Oilseed Rape",
    "CZ" = "CZ, Oil Plants*",
    "CH" = "CH, Orchards*"
  )) +
  labs(x = "Datasets",
       y = "Number of Compounds",
       fill = "Farming System")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")
