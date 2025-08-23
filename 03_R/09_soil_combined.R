#script for soil results ploting
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt
#all soil plots despite texture are ploted in this script.

#00_Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggdist)
library(GGally)
library(knitr)
library(flextable)
library(kableExtra)
library(officer)
library(FactoMineR)
library("factoextra")
library("corrplot")


#load project functions
source("./03_R/00_functions.R")


#import datasets
soil_combined <- readRDS("./01_input/soil_data_combined.rds")

#01 prepare plotting###
soil_combined_rp <- plot_prepr(soil_combined)


soil_combined_rp_short <- soil_combined_rp %>% 
  select(-c(12,15,17,19,21,23,30))

ggpairs(soil_combined_rp,columns = 5:32)

ggpairs(soil_combined_rp_short,columns = c(5,7:11,18:23,25,26),ggplot2::aes(colour=soil_combined_rp_short$farming_system, alpha = 0.5))

soil_comb_selected <- as.data.frame(soil_combined_rp_short[5:26])
ggcorrplot(soil_comb_selected, method = c("pairwise.complete.obs", "spearman"),
       label = TRUE,
       insig = "pch")

#02. basic statistical values####
summary(soil_combined$Hplus_conc_mol_l)

options(scipen=999)
#create vectors for variable categories
physical_soil_variables <- c("sample_name","clay","silt","sand","mean_BD_g_cm3")
chemical_soil_variables <- c("sample_name","pH","Hplus_conc_mol_l","Ca_aus_kation_mmol/kg",
                             "Al_aus_kation_mmol_kg","K_aus_kation_mmol_kg",
                             "Mg_aus_kation_mmol_kg","Na_aus_kation_mmol_kg",
                             "cec_NaMgCaKAl_mmol_kg","base_saturation","N%_normal",
                             "C%_normal","C/N_normal","Corg")
biological_soil_variables <- c("sample_name","microbial_c","microbial_N")

#create data frame according to variable category
soil_comb_bio <- soil_combined_rp_short %>% 
  select(all_of(biological_soil_variables))

write.xlsx(soil_comb_bio,"./02_output/12_statistical_tests/soil_comb_bio.xlsx")

soil_comb_chem <- soil_combined_rp_short %>% 
  select(all_of(chemical_soil_variables))

write.xlsx(soil_comb_chem,"./02_output/12_statistical_tests/soil_comb_chem.xlsx")


soil_comb_phys <- soil_combined_rp_short %>% 
  select(all_of(physical_soil_variables))

write.xlsx(soil_comb_phys,"./02_output/12_statistical_tests/soil_comb_phys.xlsx")
  
#create list containing all dataframes of the different variable categories
df_list_variable_category <- list(soil_comb_bio,soil_comb_chem,soil_comb_phys)
#create list of names
df_names <- c("summary_biological_variables","summary_chemical_variables","summary_physical_variables")

for (i in 1:length(df_list_variable_category)){
  
  df <- df_list_variable_category[[i]]
  
  summary_table <- df %>%
    summarise_all(list(
      Min = min, 
      Q1 = ~quantile(., 0.25), 
      Median = median, 
      Mean = mean, 
      Q3 = ~quantile(., 0.75), 
      Max = max,
      SD = sd
    )) %>% 
    pivot_longer(cols = everything(),names_to = "Parameter",values_to = "Value") %>% 
    # Separate at the LAST underscore using regex
    separate(Parameter,into = c("Parameter","Statistic"), sep= "_(?!.*_)") %>% 
    pivot_wider(names_from = "Statistic", values_from = "Value")
  
  assign(df_names[i],summary_table)
  rm(summary_table)

}


#Create list to replace variable names with proper names
parameter_names <- c(
  summary_biological_variables$Parameter,
  summary_chemical_variables$Parameter,
  summary_physical_variables$Parameter
)
parameter_names_clean <- list( "pH" = "pH","Hplus_conc_mol_l" = "Hplus (mol/l)","clay" = "Clay (%)", "silt" = "Silt (%)", "sand" = "Sand (%)",
                              "microbial_c" = "Microbial C (mg/kg)", "microbial_N" = "Microbial N (mg/kg)", "Ca_aus_kation_mmol/kg" = "CEC Ca (mmol/kg)",
                              "K_aus_kation_mmol_kg" = "CEC K (mmol/kg)", "Mg_aus_kation_mmol_kg" = "CEC Mg (mmol/kg)",
                              "Al_aus_kation_mmol_kg" = "CEC Al (mmol/kg)", "Na_aus_kation_mmol_kg" = "CEC Na (mmol/kg)", "Corg" = "Corg (%)",
                              "cec_NaMgCaKAl_mmol_kg" = "CEC NaMgCaKAl (mmol/kg)","base_saturation" = "Base saturation (%)","N%_normal" = "N (%)", "C%_normal" = "C (%)",
                              "C/N_normal" = "C/N ratio", "mean_BD_g_cm3" = "Bulk density (g/cm3)")


for (i in seq_along(df_names)) {
  # Get the original summary table by name
  summary_table <- get(df_names[i])
  
  # Apply cleaning steps
  summary_table_clean <- summary_table %>%
    mutate(across(2:8, as.numeric)) %>%
    mutate(across(where(is.numeric), ~ ifelse(Parameter == "Hplus_conc_mol_l", -log10(.), .))) %>%
    mutate_at(vars(2:8), round,digits=2) #%>%
    #mutate(Parameter = recode(Parameter, !!!parameter_names_clean))
  
  summary_table_clean <- clean_soil_names(summary_table_clean,Parameter)
  
  # Assign cleaned table to a new name
  assign(paste0(df_names[i], "_clean"), summary_table_clean)
  
  # Optional: remove temp object
  rm(summary_table, summary_table_clean,)
}

# Step 5: Create a styled flextable with embedded density plots
styled_table <- summary_physical_variables_clean %>%
  flextable()


styled_table



#4. Overview Figures soil variables ####
#prepare data set for overview plotting
soil_combined_rp_prep <- soil_combined_rp %>% 
  select(-c(29,30,15,12,17,19,21,23,32,34,35)) %>%  
  pivot_longer(cols = 5:24,names_to = "variable",values_to = "measurement")

#create vectors for variable categories
physical_soil_variables <- c("clay","silt","sand","mean_BD_g_cm3")
chemical_soil_variables <- c("pH","Hplus_conc_mol_l","Ca_aus_kation_mmol/kg",
                             "K_aus_kation_mmol_kg","Mg_aus_kation_mmol_kg",
                             "Al_aus_kation_mmol_kg","Na_aus_kation_mmol_kg",
                             "cec_NaMgCaKAl_mmol_kg","base_saturation","N%_normal","C%_normal",
                             "C/N_normal","Corg")
biological_soil_variables <- c("microbial_c","microbial_N")

soil_combined_rp_overview <- soil_combined_rp_prep %>% 
  mutate(category = case_when(
    variable %in% physical_soil_variables~"physical",
    variable %in% chemical_soil_variables~"chemical",
    variable %in% biological_soil_variables~"biological"),.after = 4)

#clean variable names ready for plotting
soil_combined_rp_overview_cn <- clean_soil_names(soil_combined_rp_overview,variable)

categories <- na.omit(c(unique(soil_combined_rp_overview_cn$category)))


#create dataframe for each category
for(name in categories){
  category_soil_variable <- soil_combined_rp_overview_cn %>% 
    filter(category == name)
  
  assign(paste0(name,"_rp_overview"),category_soil_variable)
  
  #store as output
  write.xlsx(category_soil_variable,paste0("./02_output/",name,"_soil_rp_overview",today,".xlsx"))
  
  
  
  rm(category_soil_variable)
  
  
}



ggplot(soil_physical,aes(x=substr(sample_name,1,3),y=measurement,colour = farming_system))+
  geom_boxplot()+
  facet_wrap(vars(variable),scales = "free")

ggplot(soil_physical,aes(x=substr(sample_name,1,3),y=measurement,pch=farming_system,fill = "black"))+
  geom_point(stat = "identity", position = "identity")+
  ggtitle(paste(" values on the different field plots\
          and field average"))+
 
  stat_summary(
    fun = mean, 
    geom = "point",
    aes(shape = "Mean Value"),
    shape = 8,
    color = "black",  # Color of the mean line (you can change this as needed)
    size = 2,) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45))+
  facet_wrap(vars(variable),scales = "free")
# Thickness of the line


#4.1.1 chemical soil parameters CEC#####

cec_data_rp <- chemical_rp_overview %>% 
  filter(str_starts(variable,"CEC")) %>% 
  filter(!str_detect(variable,"NaMgCa")) %>% 
  mutate(variable = substr(variable,1,6)) %>%
  mutate(variable = reorder(variable, -measurement, FUN = mean))  
  

cec_data_rp_mean <- cec_data_rp %>% 
  group_by(substr(sample_name,1,3),variable) %>% 
  mutate(mean_measurement = mean(measurement),.after = measurement) %>% 
  ungroup()


#create viridis colour scheme for the stacked bar plot
element_colour <- map_colours(cec_data_rp$variable)
cec_data_rp$color <- element_colour[as.character(cec_data_rp$variable)]

#plot CEC
#prepare labels
samples <- unique(substr(soil_combined_rp_overview_cn$sample_name,1,3))
samples <- sort(samples)
labels <- ifelse(sub("^[0-9]_([0-9]).*", "\\1", samples) == "1",
                 paste0("<b>", samples, "</b>"),
                 samples)

cec_plot <- ggplot(cec_data_rp_mean,aes(x=substr(sample_name,1,3),y = mean_measurement,
                                fill = factor(variable)))+
  geom_bar(stat = "identity",position = "stack")+
  theme_minimal()+
  ggtitle("Mean CEC per Field")+
  labs(x="Field ID",y="mmol/kg",fill="CEC Elements \n(mmol/kg)")+
  scale_fill_manual(values = element_colour)+
  scale_x_discrete(labels = labels) +
  theme(
    plot.title = element_text(size = 15,face = "bold",hjust = 0.5),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    axis.text.x = element_markdown(size = 11),
    axis.text.y = element_text(size =11)
    #legend.position = "bottom"
  )

cec_plot
ggsave("./02_output/03_KAK/effective_cec_plot.png",plot = cec_plot,
       width = 19, height=7.5, units = "cm", dpi = 300)

#4.1.2 chemical soil parameters CNS#####
cns_data_rp <- chemical_rp_overview %>% 
  filter(
    str_ends(variable, " \\(%\\)") | variable == "C/N ratio"
  ) %>% 
  filter(!str_starts(variable,"Base"))

#define plotting order of the facets
cns_data_rp$variable <- factor(cns_data_rp$variable, levels = c(
  "C (%)", "Corg (%)", "N (%)", "C/N ratio"
))

#calculate mean values of each variable per farming system
mean_lines <- cns_data_rp %>% 
  group_by(variable,farming_system) %>% 
  summarise(mean_value = mean(measurement,na.rm=TRUE),.groups = "drop")

write.xlsx(mean_lines,"./02_output/04_CNS/cns_fs_means.xlsx")

#set color for farming systems
farm_colour <- map_colours(mean_lines$farming_system)

#CNS plot
cns_plot <- ggplot(cns_data_rp,aes(x=substr(sample_name,1,3),y=measurement))+
  geom_point(size=2,alpha = 0.7,color = "#2C3E50")+
  geom_hline(
    data = mean_lines,
    aes(yintercept = mean_value, color = farming_system),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_color_manual(values = fs_colour,labels = c("Regenerative","Conventional"))+
  scale_x_discrete(labels = labels) +
  ggtitle("CNS measurements per field")+
  labs(x="Field ID",y="",color ="Farming System Mean:")+
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15,face = "bold",hjust = 0.5),
    axis.title.x = element_text(size=13),
    axis.title.y = element_text(size=13),
    axis.text.x = element_markdown(size = 11,angle = 45,hjust = 1),
    axis.text.y = element_text(size =11),
    strip.text = element_text(size = 13),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 13),
    legend.position = "bottom"
  )+
  facet_wrap(vars(variable),scales = "free_y")
ggsave("./02_output/04_CNS/cns_results.png",plot = cns_plot,
       width = 19, height=13, units = "cm", dpi = 300)

#4.1.3 chemical soil parameters pH#####
#create pH dataset
ph_data_rp <- chemical_rp_overview %>% 
  filter(variable == "pH")

#calculate mean values of each variable per farming system
mean_lines_ph <- ph_data_rp %>%
  mutate(Hplus = 10^(-measurement)) %>%
  group_by(variable, farming_system) %>%
  summarise(
    mean_value = -log10(mean(Hplus, na.rm = TRUE)),
    .groups = "drop"
  )

write.xlsx(mean_lines_pH,"./02_output/05_pH/pH_fs_means.xlsx")
  

ph_plot <- thesis_plot(ph_data_rp,ph_data_rp$sample_name,ph_data_rp$measurement,mean_lines_ph)+
  scale_x_discrete(labels = labels) +
  ggtitle("pH measurements per field")+
  labs(y="pH value")

ggsave("./02_output/05_pH/pH_results.png",plot = ph_plot,
       width = 19, height=9, units = "cm", dpi = 300)


#4.2.1 biological soil parameter microbial biomass####


#adjust variable names
biological_rp_overview <- biological_rp_overview %>% 
  mutate(variable = substr(variable,1,11))

#calculate mean values per farming system
mean_lines <- biological_rp_overview %>% 
  group_by(variable,farming_system) %>% 
  summarise(mean_value = mean(measurement,na.rm=TRUE),.groups = "drop")

write.xlsx(mean_lines,"./02_output/06_biological/biological_fs_means.xlsx")




biological_plot <- thesis_plot(biological_rp_overview,biological_rp_overview$sample_name,
                               biological_rp_overview$measurement,mean_lines)+
  scale_x_discrete(labels = labels) +
  ggtitle("Microbial Biomass per field")+
  labs(y="mg/kg")+
  facet_wrap(vars(variable),scales = "free_y")


ggsave("./02_output/06_biological/bio_results.png",plot = biological_plot,
       width = 19, height=9, units = "cm", dpi = 300)

biological_plot


#4.3.1 physical soil parameter bulk density####
bulk_density_data_rp <- physical_rp_overview %>%
  filter(variable == "Bulk density (g/cm3)") %>% 
  mutate(variable = substr(variable,1,12))

#calculate mean values per farming system
mean_lines <- bulk_density_data_rp %>% 
  group_by(variable,farming_system) %>% 
  summarise(mean_value = mean(measurement,na.rm=TRUE),.groups = "drop")

write.xlsx(mean_lines,"./02_output/07_bulk_density/bulk_density_fs_means.xlsx")

#plot bulk density
bulk_density_plot <- thesis_plot(bulk_density_data_rp,bulk_density_data_rp$sample_name,
                                 bulk_density_data_rp$measurement,mean_lines)+
  scale_x_discrete(labels = labels) +
  ggtitle("Bulk density per field")+
  labs(y="g/cm3")

bulk_density_plot

ggsave("./02_output/07_bulk_density/bulk_density_results.png",plot = bulk_density_plot,
       width = 19, height=9, units = "cm", dpi = 300)
# ggplot(soil_physical,aes(x=variable,y=measurement))+
#   stat_halfeye(alpha=0.5)+
#   stat_interval(.width=c(0.5,0.75,0.95),alpha=0.3)+
#   stat_dots()+
#   stat_summary(geom = "point",fun = median)+
#   coord_flip()
#  # facet_wrap(~variable,scales = "free")
#   # geom_weave()+
# 
# ggplot(soil_physical, aes(x = measurement)) +  # Map density to x-axis
#   stat_halfeye(fill_type = "segments", alpha = 0.5) +  # Half-eye density
#   stat_interval(.width = c(0.5, 0.75, 0.95), alpha = 0.3) +  # Confidence intervals
#   #stat_summary(geom = "point", fun = median, color = "black", size = 3) +  # Median point
#   facet_wrap(~ variable, scales = "free_x") +  # Facet by variable with free scales
#   theme_minimal() +
#   labs(x = "Measurement", y = "Density", title = "Density Distributions of Soil Variables")
# 
# ggplot(soil_physical,aes(variable,measurement))+
#   stat_halfeye(fill_type = "segments", alpha=0.)+
#   coord_flip()+
#   facet_wrap(~variable)
#   stat_interval()
#   
# ggplot(soil_physical, aes(x = measurement, fill = farming_system)) +
#     stat_halfeye(
#       aes(y = 0),        # place the half-eye around y=0
#       orientation = "horizontal",
#       adjust = 0.25,
#       .width = c(0.5, 0.8, 0.95),
#       alpha = 0.4
#     )+
#   stat_interval()+
#   stat_dots()
#     facet_wrap(~ variable, scales = "free") +
#     theme_minimal() +
#     labs(
#       x = "Measurement",
#       y = NULL,
#       title = "Facet-Wrapped Half-Eyes by Variable"
#     )
