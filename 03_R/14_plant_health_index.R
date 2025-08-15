#script to get plant health index simillar to soil health index
#Code created by Florian Christ as part of the Master Thesis 30.07.2025
#used R Version. 4.4.1 2024-06-14 ucrt


#00_Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)



#load project functions
source("./03_R/00_functions.R")


#load data
plant_data_combined_incl_yield <-  readRDS("./01_input/combined_plant_data_incl_yield.rds")
plant_data_combined <- readRDS("./01_input/combined_plant_data.rds")

write.xlsx(plant_data_combined,"./01_input/plant_data_combined.xlsx")
#01. create PHI data frame####
#clean combined plant data for index creation
plant_data_combined_incl_yield_clean <- plant_data_combined_incl_yield %>% 
  select(-c(2:7,13,14))
plant_data_combined_incl_yield_clean_rp <- plot_prepr(plant_data_combined_incl_yield_clean)
plant_data_combined_incl_yield_clean_rp <- plant_data_combined_incl_yield_clean_rp %>% 
  select(-shape,-color)



#02. calculate
#score plant parameters
scored_plant_data <- score_phi(plant_data_combined_incl_yield_clean_rp)

phi_total <- scored_plant_data %>% 
  mutate(phi_total = rowSums(across(5:9), na.rm = TRUE)) %>% 
  mutate(phi_total_no_yield = rowSums(across(5:8), na.rm = TRUE))


#calculate mean per field
phi_mean <- phi_total %>% 
  group_by(location,farming_system) %>% 
  summarise(across(
    .cols = c("Chlorophyll (SPAD)","Plant height (cm)",
              "Root health score","Specific Leaf Area (g/cm2)",
              "Yield (dt/ha)","phi_total","phi_total_no_yield"),
    .fns = ~mean(.x, na.rm = TRUE),
    .names = "{.col}"
  ), .groups = "drop")

phi_mean_clean <- phi_mean %>% 
  mutate("Field ID" = paste(location,farming_system,sep = "_"),.before = 1)



write.xlsx(phi_mean_total,"./02_output/09_plant_health/phi_mean_total.xlsx")

#03. prepare for plotting#####
#rearange data
phi_mean_clean_prep <- phi_mean_clean %>% 
  pivot_longer(cols=4:8,names_to = "variable",values_to = "PHI score")


phi_cleeveland <- phi_mean_clean_prep %>% 
  select(-`Field ID`) %>% 
  pivot_wider(names_from = farming_system, values_from = `PHI score`)
phi_cleeveland <- phi_cleeveland %>% 
  rename("Regenerative" = 5, "Conventional" = 6) %>% 
  mutate(across(5:6,as.numeric))


#4. generat cleeveland dot plot####
location_labels <- c(
  "1" = "Freudwil (R = 2.61)",
  "2" = "Niederhasli (R = 3.94, C = 2.59)",
  "3" = "Läufelfingen (R = 3.99, C = 3.75)",
  "4" = "Heimenhaus. (R = 3.18, C = 3.50)",
  "5" = "Heimiswil (R = 2.52, C = 3.41)",
  "6" = "Ueberstorf (R = 1.59, C = 3.30)"
)

# 1) One label per location
loc_labels <- phi_cleeveland %>%
  group_by(location) %>%
  summarise(
    r_better = sum(Regenerative > Conventional, na.rm = TRUE),
    c_better = sum(Conventional > Regenerative, na.rm = TRUE),
    n_vars   = sum(!is.na(Regenerative) & !is.na(Conventional)),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("R vs. C: %d-%d", r_better, c_better)) %>% 
  mutate(label = if_else(label=="Reg vs. Conv: 0-0","no comp.", label))


phi_plot <- ggplot(phi_cleeveland) +
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

phi_plot
ggsave("./02_output/09_plant_health/phi_cleeveland_plot.png",plot = phi_plot,
       width = 19, height=27, units = "cm", dpi = 300)
