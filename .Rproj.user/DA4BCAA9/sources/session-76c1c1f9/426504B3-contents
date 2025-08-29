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
soil_risk <- readRDS("./02_output/11_ppp_comparison/ppp_all_cleaned_long_meta_rq.rds")
#write.xlsx(soil_combined,"./02_output/soil_data_combined.xlsx")

#1. create SHI dataframe ####
#first preparation step
soil_combined_rp <- plot_prepr(soil_combined)
#select all the parameters defined in the MDS minmum data set
shi_dataset_basic <- soil_combined_rp %>% 
  select(1:4,6,10,18,22,27,33) #Risk Quotient is yet missing, needs to be added ASAP!!!!

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



write.xlsx(shi_dataset_basic_ph,"./02_output/08_SHI/shi_basic_data_no_CNratio.xlsx")


#1.2 Prepare RQ data for SHI
soil_risk_cum_plot <- soil_risk %>% 
  group_by(sample) %>% 
  summarise(cumulative_RQ = sum(risk_quotient,na.rm = TRUE),
            .groups = "drop") %>%
  rename("sample_name" = "sample")


soil_risk_cum_plot_classified <- soil_risk_cum_plot %>% 
  mutate(rq_classified = 
           case_when(
    cumulative_RQ < 0.01 ~ 1,
    cumulative_RQ >= 0.01 & cumulative_RQ < 0.1 ~ 0.7,
    cumulative_RQ >= 0.1 & cumulative_RQ < 1 ~ 0.3,
    cumulative_RQ > 1 ~0,
    TRUE ~ cumulative_RQ)
    )

#2. calculate SHI scores####
scored_soil_data <- score_shi(shi_dataset_basic_ph)

scored_soil_data_rq <- scored_soil_data %>% 
  left_join(soil_risk_cum_plot_classified, by = "sample_name") %>% 
  select(-cumulative_RQ)

scored_soil_data_rq_total <- scored_soil_data_rq %>% 
  mutate(shi_total = rowSums(across(5:12),na.rm = TRUE))

write.xlsx(scored_soil_data_rq_total,"./02_output/08_SHI/shi_total_rq_no_CNratio.xlsx")

#calculate mean per field
shi_mean <- scored_soil_data %>% 
  group_by(location,farming_system) %>% 
  summarise(across(
    .cols = c("pH", "microbial_c", "Mg_aus_kation_mmol_kg", 
              "Na_aus_kation_mmol_kg", "C%_normal", "mean_BD_g_cm3"),
    .fns = mean,
    .names = "{.col}"
  ), .groups = "drop")

shi_mean_clean <- shi_mean %>% 
  mutate("Field ID" = paste(location,farming_system,sep = "_"),.before = 1) 


shi_mean_total <- shi_mean_clean %>% 
  mutate(shi_total = rowSums(across(4:9), na.rm = TRUE))


shi_mean_rq_total <- scored_soil_data_rq_total %>% 
  group_by(location,farming_system) %>% 
  summarise(across(
    .cols = c("pH", "microbial_c", "Mg_aus_kation_mmol_kg", 
              "Na_aus_kation_mmol_kg", "C%_normal", 
              "mean_BD_g_cm3", "rq_classified", "shi_total"),
    .fns = mean,
    .names = "{.col}"
  ),
  .groups = "drop") %>% 
  mutate("Field ID" = paste(location,farming_system,sep = "_"),.before = 1)

write.xlsx(shi_mean_total,"./02_output/08_SHI/shi_mean_total_no_CNratio.xlsx")
write.xlsx(shi_mean_rq_total,"./02_output/08_SHI/shi_mean_total_rq_no_CNratio.xlsx")

#prepare for ploting
#rearange data
shi_mean_clean_prep <- shi_mean_clean %>% 
  pivot_longer(cols=4:9,names_to = "variable",values_to = "SHI score")

shi_mean_clean_prep_rn <- clean_soil_names(shi_mean_clean_prep,variable)

shi_cleeveland <- shi_mean_clean_prep_rn %>% 
  select(-`Field ID`) %>% 
  pivot_wider(names_from = farming_system, values_from = `SHI score`)
shi_cleeveland <- shi_cleeveland %>% 
  rename("Regenerative" = 3, "Conventional" = 4)


shi_mean_rq_total_prep <- shi_mean_rq_total %>%
  rename("RQ score" = "rq_classified") %>% 
  pivot_longer(cols = 4:11, names_to = "variable", values_to = "SHI score")

shi_mean_rq_total_prep <- clean_soil_names(shi_mean_rq_total_prep,variable)

shi_cleeveland_rq <- shi_mean_rq_total_prep %>% 
  select(-`Field ID`) %>% 
  pivot_wider(names_from = farming_system, values_from = `SHI score`)
shi_cleeveland_rq <- shi_cleeveland_rq %>% 
  rename("Regenerative" = 3, "Conventional" = 4)

shi_cleeveland_rq_plot <- shi_cleeveland_rq %>% 
  filter(variable != "shi_total")


# 1) One label per location
loc_labels <- shi_cleeveland_rq_plot %>%
  group_by(location) %>%
  summarise(
    r_better = sum(Regenerative > Conventional, na.rm = TRUE),
    c_better = sum(Conventional > Regenerative, na.rm = TRUE),
    n_vars   = sum(!is.na(Regenerative) & !is.na(Conventional)),
    .groups = "drop"
  ) %>%
  mutate(label = sprintf("R vs. C: %d-%d", r_better, c_better)) %>% 
  mutate(label = if_else(label=="Reg vs. Conv: 0-0","no comp.", label))



#3. generat cleeveland dot plot
location_labels <- c(
  "1" = "Freudwil (R = 5.35) <sup>1.)</sup>",
  "2" = "Niederhasli (R = 4.23, C = 3.45) <sup>2.)</sup>",
  "3" = "Läufelfingen (R = 6.27, C = 5.72)",
  "4" = "Heimenhaus. (R = 5.44, C = 3.81)",
  "5" = "Heimiswil (R = 4.84, C = 4.00)",
  "6" = "Ueberstorf (R = 4.83, C = 3.62)"
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


ggsave("./02_output/08_shi/shi_cleeveland_plot_no_CNratio.png",plot = shi_plot,
       width = 19, height=27, units = "cm", dpi = 300)



#final rq plot



shi_plot_rq <- ggplot(shi_cleeveland_rq_plot) +
  geom_segment(aes(x = Regenerative, xend = Conventional,
                   y = variable, yend = variable), color = "#2C3E50",alpha=0.7) +
  geom_point(aes(x = Regenerative, y = variable), color = "#66c2a5", size = 3) +
  geom_point(aes(x = Conventional, y = variable), color = "#fc8d62", size = 3) +
  facet_wrap(~location, scales = "free_x",nrow = 3,
             labeller = as_labeller(location_labels)) +
  scale_x_continuous(limits = c(0,1))+
  
  # bottom-left annotation per facet
  geom_richtext(data = loc_labels,
            aes(x = 0.0, y = -Inf, label = label),
            hjust = 0, vjust = -0.6, size = 3.5,
            inherit.aes = FALSE,
            fill = "transparent") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 15,face = "bold",hjust = 0.5),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size =11),
    strip.text.x = element_markdown(size = 13),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )+
  labs(
    title = "Cleveland Dot Plot of SHI Scores by Variable and Location",
    x = "Score",
    y = "Soil Health Parameter"
  )
shi_plot_rq


ggsave("./02_output/08_shi/shi_cleeveland_plot_MA_no_CNratio.png",plot = shi_plot_rq,
       width = 19, height=27, units = "cm", dpi = 300)




shi_mean_rq_total <- scored_soil_data_rq_total %>% 
  group_by(farming_system) %>% 
  summarise(across(
    .cols = c("pH", "microbial_c", "Mg_aus_kation_mmol_kg", 
              "Na_aus_kation_mmol_kg", "C%_normal", 
              "mean_BD_g_cm3", "rq_classified", "shi_total"),
    .fns = median,
    .names = "{.col}"
  ),
  .groups = "drop") 
