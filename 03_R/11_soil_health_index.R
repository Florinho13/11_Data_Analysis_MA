#script for soil health index scoring and ploting
#Code created by Florian Christ as part of the Master Thesis 12.04.2025
#used R Version. 4.4.1 2024-06-14 ucrt
#all soil plots despite texture are ploted in this script.


#00_Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)



#load project functions
source("./03_R/00_functions.R")


#import datasets
soil_combined <- readRDS("./01_input/soil_data_combined.rds")
write.xlsx(soil_combined,"./02_output/soil_data_combined.xlsx")

#1. create SHI dataframe ####
#first preparation step
soil_combined_rp <- plot_prepr(soil_combined)
#select all the parameters defined in the MDS minmum data set
shi_dataset_basic <- soil_combined_rp %>% 
  select(1:4,6,10,18,22,27,28,33) #Risk Quotient is yet missing, needs to be added ASAP!!!!

shi_dataset_basic_ph <- shi_dataset_basic %>% 
  mutate(pH = -log10(Hplus_conc_mol_l)) %>% 
  select(-Hplus_conc_mol_l)

#check for outliers in the dataset
shi_dataset_basic_ph_long <- shi_dataset_basic_ph %>% 
  pivot_longer(cols = 5:11,names_to = "variable",values_to = "measurement")

variables <- unique(shi_dataset_basic_ph_long$variable)

for(i in variables){
  shi_dataset_basic_ph_long_f <- shi_dataset_basic_ph_long %>% 
    filter(variable == i)
  
  p <- ggplot(shi_dataset_basic_ph_long_f,aes(x=variable,y=measurement))+
  geom_boxplot()
  
  print(p)
}



write.xlsx(shi_dataset_basic_ph,"./02_output/08_SHI/shi_basic_data.xlsx")


#2. calculate SHI scores####
scored_soil_data <- score_shi(shi_dataset_basic_ph)


#calculate mean per field
shi_mean <- scored_soil_data %>% 
  group_by(location,farming_system) %>% 
  summarise(across(
    .cols = c("pH", "microbial_c", "Mg_aus_kation_mmol_kg", 
              "Na_aus_kation_mmol_kg", "C%_normal", "C/N_normal", "mean_BD_g_cm3"),
    .fns = mean,
    .names = "{.col}"
  ), .groups = "drop")

shi_mean_clean <- shi_mean %>% 
  mutate("Field ID" = paste(location,farming_system,sep = "_"),.before = 1)


shi_mean_total <- shi_mean_clean %>% 
  mutate(shi_total = rowSums(across(4:10), na.rm = TRUE))

write.xlsx(shi_mean_total,"./02_output/08_SHI/shi_mean_total.xlsx")

#prepare for ploting
#rearange data
shi_mean_clean_prep <- shi_mean_clean %>% 
  pivot_longer(cols=4:10,names_to = "variable",values_to = "SHI score")

shi_mean_clean_prep_rn <- clean_soil_names(shi_mean_clean_prep,variable)

shi_cleeveland <- shi_mean_clean_prep_rn %>% 
  select(-`Field ID`) %>% 
  pivot_wider(names_from = farming_system, values_from = `SHI score`)
shi_cleeveland <- shi_cleeveland %>% 
  rename("Regenerative" = 3, "Conventional" = 4)


#3. generat cleeveland dot plot
location_labels <- c(
  "1" = "Freudwil",
  "2" = "Niederhasli",
  "3" = "Läufelfingen",
  "4" = "Heimenhausen",
  "5" = "Heimiswil",
  "6" = "Ueberstorf"
)


shi_plot <- ggplot(shi_cleeveland) +
  geom_segment(aes(x = Regenerative, xend = Conventional,
                   y = variable, yend = variable), color = "#2C3E50",alpha=0.7) +
  geom_point(aes(x = Regenerative, y = variable), color = "#66c2a5", size = 3) +
  geom_point(aes(x = Conventional, y = variable), color = "#fc8d62", size = 3) +
  facet_wrap(~location, scales = "free_x",nrow = 3,
             labeller = as_labeller(location_labels)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15,face = "bold",hjust = 0.5),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size =11),
    strip.text = element_text(size = 13),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )+
  labs(
    title = "Cleveland Dot Plot of SHI Scores by Variable and Location",
    x = "Score (0–1)",
    y = "Soil Health Parameter"
  )
shi_plot
ggsave("./02_output/08_shi/shi_cleeveland_plot.png",plot = shi_plot,
       width = 19, height=27, units = "cm", dpi = 300)

