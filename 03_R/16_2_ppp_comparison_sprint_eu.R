#script to get evaluation of ppp measurements in soil
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt

#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggtext)
library(hrbrthemes)
library(patchwork)
library(flextable)
library(officer)
library(ggpubr)
library(rstatix)



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

#01.2 get total_substances for both datasets #####

sprint_ppp_soil_long_meta_freq_plot_cz <- sprint_ppp_soil_long_meta %>% 
  group_by(sample, farming_system) %>% 
  summarise(n_detected = sum(detected,na.rm = TRUE), .groups = "drop") %>% 
  mutate(farming_system = str_replace(farming_system, "Conventional","2"),
         farming_system = str_replace(farming_system, "Organic","3")) %>% 
  mutate(dataset = substr(sample,1,2)) %>% 
  filter(dataset %in% c("CZ"))

sprint_ppp_soil_long_meta_freq_plot_eu <- sprint_ppp_soil_long_meta %>% 
  group_by(sample, farming_system) %>% 
  summarise(n_detected = sum(detected,na.rm = TRUE), .groups = "drop") %>% 
  mutate(farming_system = str_replace(farming_system, "Conventional","2"),
         farming_system = str_replace(farming_system, "Organic","3")) %>% 
  mutate(dataset = "EU")

thesis_ppp_soil_long_meta_freq_plot <- ppp_all_cleaned_long_meta_rq %>% 
  group_by(sample,farming_system) %>% 
  summarise(n_detected = sum(detected,na.rm = TRUE), .groups = "drop") %>% 
  mutate(dataset = "Thesis")

sprint_thesis_combined_freq_plot_eu <- thesis_ppp_soil_long_meta_freq_plot %>% 
  bind_rows(sprint_ppp_soil_long_meta_freq_plot_cz,sprint_ppp_soil_long_meta_freq_plot_eu)

saveRDS(sprint_thesis_combined_freq_plot_eu,"./02_output/11_ppp_comparison/sprint_thesis_combined_freq_plot_eu")
write.xlsx(sprint_thesis_combined_freq_plot_eu,"./02_output/11_ppp_comparison/sprint_thesis_combined_freq_plot_eu.xlsx")

pw_wilcox_det <- sprint_thesis_combined_freq_plot_eu %>%
  group_by(dataset) %>%
  pairwise_wilcox_test(n_detected ~ farming_system, p.adjust.method = "BH")

pw_wilcox_det_pos <- pw_wilcox_det %>%
  add_xy_position(x = "dataset", dodge = 0.9)

comparison_boxplot_number_of_detections <- ggplot(data = sprint_thesis_combined_freq_plot_eu,
       aes(x = dataset,y = n_detected, fill = farming_system))+
  geom_boxplot() +
  stat_boxplot(geom ='errorbar',position = position_dodge(width = 0.9)) +
  ggtitle("Total Number of Detected Substances on Investigated Plots from SPRINT & Thesis")+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    linetype = "dashed",
    width = 0.5,        # diamond shape
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
       y = "Number of Substances",
       fill = "Farming System")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")+
  stat_pvalue_manual(
    pw_wilcox_det_pos %>% filter(p.adj <= 0.05),
    label = "p.adj.signif",   # "***", "**", "*"
    hide.ns = TRUE,
    tip.length = 0.01,
    bracket.size = 0.4,
    step.increase = 0.07,
    inherit.aes = FALSE
  )

comparison_boxplot_number_of_detections


#1.3 get total concentrations of both datasets####
sprint_ppp_soil_long_meta_tot_conc_plot_eu <- sprint_ppp_soil_long_meta %>% 
  group_by(sample, farming_system) %>% 
  summarise(total_concentrations = sum(concentrations_ng_g,na.rm = TRUE), .groups = "drop") %>% 
  mutate(farming_system = str_replace(farming_system, "Conventional","2"),
         farming_system = str_replace(farming_system, "Organic","3")) %>% 
  mutate(dataset = substr(sample,1,2)) %>% 
  filter(dataset %in% c("CZ"))

sprint_ppp_soil_long_meta_tot_conc_plot_eu <- 

thesis_ppp_soil_long_meta_tot_conc_plot <- ppp_all_cleaned_long_meta_rq %>% 
  group_by(sample,farming_system) %>% 
  summarise(total_concentrations = sum(concentrations_ng_g,na.rm = TRUE), .groups = "drop") %>% 
  mutate(dataset = "Thesis")

sprint_thesis_combined_tot_conc_plot <- thesis_ppp_soil_long_meta_tot_conc_plot %>% 
  bind_rows(sprint_ppp_soil_long_meta_tot_conc_plot)

saveRDS(sprint_thesis_combined_tot_conc_plot,"./02_output/11_ppp_comparison/sprint_thesis_combined_tot_conc_plot")
write.xlsx(sprint_thesis_combined_tot_conc_plot,"./02_output/11_ppp_comparison/sprint_thesis_combined_tot_conc_plot.xlsx")

pw_wilcox_conc <- sprint_thesis_combined_tot_conc_plot %>%
  group_by(dataset) %>%
  pairwise_wilcox_test(total_concentrations ~ farming_system, p.adjust.method = "BH")

# pw_wilcox_conc_pos <- pw_wilcox_conc %>%
#   add_xy_position(x = "dataset", dodge = 0.9)

comparison_boxplot_tot_concentrations <- ggplot(data = sprint_thesis_combined_tot_conc_plot,
                                                  aes(x = dataset,y = total_concentrations, fill = farming_system))+
  geom_boxplot() +
  stat_boxplot(geom ='errorbar',position = position_dodge(width = 0.9)) +
  ggtitle("Total Concentration of Detected Substances on Investigated Plots from SPRINT & Thesis")+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    linetype = "dashed",
    width = 0.5,        # diamond shape
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
  scale_y_continuous(trans = "log10")+
  labs(x = "Datasets",
       y = "Total Concentrations [ng/g]",
       fill = "Farming System")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")

comparison_boxplot_tot_concentrations

#1.4 get cumulative Risk Quotient of both datasets #######
#multiplicate pnec (mg/kg) with 1000 in order to come to (ng/g)
pnec_soil_data_clean <- pnec_soil_data %>% 
  mutate(pnec_soil_ng_g = as.numeric(`PNEC_soil.(mg/kg)`)*1000) %>% 
  select(-`PNEC_soil.(mg/kg)`)

#prepare rq for SPRINT data
sprint_ppp_soil_all_cleaned_long_meta_pnec <- sprint_ppp_soil_long_meta %>% 
  left_join(pnec_soil_data_clean,by = join_by(ppp_compound == PPP_compound_clean))

sprint_ppp_soil_all_cleaned_long_meta_pnec_rq <- sprint_ppp_soil_all_cleaned_long_meta_pnec %>% 
  mutate(risk_quotient = concentrations_ng_g/pnec_soil_ng_g)

sprint_ppp_soil_long_meta_rq_plot <- sprint_ppp_soil_all_cleaned_long_meta_pnec_rq %>% 
  group_by(sample, farming_system) %>% 
  summarise(cumulative_rq = sum(risk_quotient,na.rm = TRUE), .groups = "drop") %>% 
  mutate(farming_system = str_replace(farming_system, "Conventional","2"),
         farming_system = str_replace(farming_system, "Organic","3")) %>% 
  mutate(dataset = substr(sample,1,2)) %>% 
  filter(dataset %in% c("CZ","CH"))

thesis_ppp_soil_long_meta_rq_plot <- ppp_all_cleaned_long_meta_rq %>% 
  group_by(sample,farming_system) %>% 
  summarise(cumulative_rq = sum(risk_quotient,na.rm = TRUE), .groups = "drop") %>% 
  mutate(dataset = "Thesis")

sprint_thesis_combined_rq_plot <- thesis_ppp_soil_long_meta_rq_plot %>% 
  bind_rows(sprint_ppp_soil_long_meta_rq_plot)

pw_wilcox_rq <- sprint_thesis_combined_rq_plot %>%
  group_by(dataset) %>%
  pairwise_wilcox_test(cumulative_rq ~ farming_system, p.adjust.method = "BH")

# # pw_wilcox_rq_pos <- pw_wilcox_rq %>%
#   add_xy_position(x = "dataset", dodge = 0.9)

comparison_boxplot_cumulative_rq <- ggplot(data = sprint_thesis_combined_rq_plot,
                                                  aes(x = dataset,y = cumulative_rq, fill = farming_system))+
  geom_boxplot() +
  stat_boxplot(geom ='errorbar',position = position_dodge(width = 0.9)) +
  ggtitle("Cumulative Risk Quotient on Investigated Plots from SPRINT & Thesis")+
  stat_summary(
    fun = mean,
    geom = "crossbar",
    linetype = "dashed",
    width = 0.5,        # diamond shape
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
  scale_y_continuous(trans = "log10")+
  labs(x = "Datasets",
       y = "Cumulative Risk Quotient",
       fill = "Farming System")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")

comparison_boxplot_cumulative_rq


options(scipen = 999)
combined_comparison_plot <- comparison_boxplot_number_of_detections +  
  #axis.text.x  = element_blank()) 
  comparison_boxplot_tot_concentrations +
  #axis.text.x  = element_blank()) 
  comparison_boxplot_cumulative_rq +
  plot_layout(ncol = 1, 
              heights = c(1, 1, 1), 
              guides ="collect") +
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position = "bottom",
        plot.title = element_blank())

combined_comparison_plot



ggsave(
  "./02_output/11_ppp_comparison/comparison_boxplots_sprint_thesis.png",   # or .jpg
  combined_comparison_plot,
  width = 19,               # cm (slightly smaller than full A4 to allow margins)
  height = 27,              # cm
  units = "cm",
  dpi = 300                 # 300 dpi = print quality
)

combined_plot

#2. between groups comparison wilcox####
df <- sprint_thesis_combined_freq_plot

# Combine factors
df <- df %>% mutate(group = interaction(farming_system, dataset, sep = "_"))


# Pairwise Wilcoxon across all system–dataset combos
pw_all_det <- pairwise.wilcox.test(df$n_detected, df$group, p.adjust.method = "BH")
pw_all_det

df <- sprint_thesis_combined_tot_conc_plot

# Combine factors
df <- df %>% mutate(group = interaction(farming_system, dataset, sep = "_"))

# Pairwise Wilcoxon across all system–dataset combos
pw_all_conc <- pairwise.wilcox.test(df$total_concentrations, df$group, p.adjust.method = "BH")
pw_all_conc


df <- sprint_thesis_combined_rq_plot

# Combine factors
df <- df %>% mutate(group = interaction(farming_system, dataset, sep = "_"))

# Pairwise Wilcoxon across all system–dataset combos
pw_all_rq <- pairwise.wilcox.test(df$cumulative_rq, df$group, p.adjust.method = "BH")
pw_all_rq


#3. statistical key figures
#substances detected
key_figures_det <- sprint_thesis_combined_freq_plot %>% 
  group_by(dataset,farming_system) %>% 
  summarise(med = median(n_detected,na.rm = TRUE),
            average = mean(n_detected,na.rm = TRUE),
            minimum = min(n_detected,na.rm = TRUE),
            maximum = max(n_detected,na.rm = TRUE),
            sd = sd(n_detected,na.rm = TRUE))

key_figures_det_dataset <- sprint_thesis_combined_freq_plot %>% 
  group_by(dataset) %>% 
  summarise(med = median(n_detected,na.rm = TRUE),
            average = mean(n_detected,na.rm = TRUE),
            minimum = min(n_detected,na.rm = TRUE),
            maximum = max(n_detected,na.rm = TRUE),
            sd = sd(n_detected,na.rm = TRUE))

#total concentrations
key_figures_conc <- sprint_thesis_combined_tot_conc_plot %>% 
  group_by(dataset,farming_system) %>% 
  summarise(med = median(total_concentrations,na.rm = TRUE),
            average = mean(total_concentrations,na.rm = TRUE),
            minimum = min(total_concentrations,na.rm = TRUE),
            maximum = max(total_concentrations,na.rm = TRUE),
            sd = sd(total_concentrations,na.rm = TRUE))


key_figures_conc_dataset <- sprint_thesis_combined_tot_conc_plot %>% 
  group_by(dataset) %>% 
  summarise(med = median(total_concentrations,na.rm = TRUE),
            average = mean(total_concentrations,na.rm = TRUE),
            minimum = min(total_concentrations,na.rm = TRUE),
            maximum = max(total_concentrations,na.rm = TRUE),
            sd = sd(total_concentrations,na.rm = TRUE))

#cumulative RQ
key_figures_rq <- sprint_thesis_combined_rq_plot %>% 
  group_by(dataset,farming_system) %>% 
  summarise(med = median(cumulative_rq,na.rm = TRUE),
            average = mean(cumulative_rq,na.rm = TRUE),
            minimum = min(cumulative_rq,na.rm = TRUE),
            maximum = max(cumulative_rq,na.rm = TRUE),
            sd = sd(cumulative_rq,na.rm = TRUE))

key_figures_rq_dataset <- sprint_thesis_combined_rq_plot %>% 
  group_by(dataset) %>% 
  summarise(med = median(cumulative_rq,na.rm = TRUE),
            average = mean(cumulative_rq,na.rm = TRUE),
            minimum = min(cumulative_rq,na.rm = TRUE),
            maximum = max(cumulative_rq,na.rm = TRUE),
            sd = sd(cumulative_rq,na.rm = TRUE))
