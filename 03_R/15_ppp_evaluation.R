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



#load project functions
source("./03_R/00_functions.R")


#load data
ppp_soil_all <- readRDS("./01_input/PPP_results_clean_2025_02_14.rds")
ppp_metadata_sprint_coding <- read.xlsx("./01_input/Metadata_All_CSS_EHA_and_PPP_coding_Aug_2023.xlsx",sheet = 5)
pnec_soil_data <- read.xlsx("./01_input/PNEC_soil_water_list_120722.xlsx", sheet = 2)[,c(1,24)]
ppp_info_set <- read.xlsx("./01_input/PPP_Info_D2_3.xlsx")


dir.create("./02_output/10_ppp")
output_dir <- "./02_output/10_ppp/"

#01. clean ppp names####
clean_ppp_names <- ppp_metadata_sprint_coding$R_Database_Name
pnec_ppp_names <- pnec_soil_data$PPP_compound
current_ppp_names <- colnames(ppp_soil_all[2:194])
#setdiff(clean_ppp_names,current_ppp_names)

#clean names
current_ppp_names <- str_replace(current_ppp_names,".Results","")
current_ppp_names <- str_replace(current_ppp_names,"FMOC-","")
current_ppp_names <- str_replace_all(current_ppp_names,"-","_")
current_ppp_names <- str_replace_all(current_ppp_names,"\\.","_")
current_ppp_names <- str_replace_all(current_ppp_names,"\\(","_")
current_ppp_names <- str_replace_all(current_ppp_names,"\\)","")
current_ppp_names <- str_replace_all(current_ppp_names,"\\,","_")
current_ppp_names <- str_replace_all(current_ppp_names,"__","_")

#name map
name_map <- c(
  "o_p'_DDD" = "DDD_o_p",
  "p_p'_DDD" = "DDD_p_p",
  "o_p'_DDE" = "DDE_o_p",
  "p_p'_DDE" = "DDE_p_p",
  "o_p'_DDT" = "DDT_o_p",
  "p_p'_DDT" = "DDT_p_p",
  "Atrazin"  = "Atrazine",
  "Bentazon" = "Bentazone",
  "Azoxystrobin_O_demethyl_CyPM" = "Azoxystrobin_O_demethyl",
  "Bixafen_metabolite_desmethyl" = "Bixafen_desmethyl",
  "c_HCH Lindane" = "Lindane_gamma_HCH",
  "Captan TPHI" = "Captan_THPI_1_2_3_6_tetrahydrophthalimide",
  "Chlorpyrifo_methyl_TCPy" = "Chlorpyrifos_methyl_TCPy",
  "Clomazon" = "Clomazone",
  "Cyflufenamid" = "Cyflufenamide",
  "Cyfluthrin" = "Cyfluthrin_beta_cyfluthrin",
  "Cyprodinil_metabolite_CGA304075" = "Cyprodinil_metabolite",
  "Diledrin" = "Dieldrin",
  "Emamectin_B1a" = "Emamectin",
  "Fenbuconazol" = "Fenbuconazole",
  "Fluazifop_P_free" = "Fluazifop_P_only_free",
  "glyphosate" = "Glyphosate",
  "HCB" = "Hexachlorobenzene",
  "Imidacloprid_desnitro_" = "Imidacloprid_desnitro",
  "lambda_Cyhalothrin" = "Lambda_Cyhalothrin",
  "Metalaxyl_Metabolite_CGA_62826_87764_37_2" = "Metalaxyl_Metabolite",
  "Metconazol" = "Metconazole",
  "Methoxyfenozid" = "Methoxyfenozide",
  "Metolachlor_ethane_sulfonic_acid_ESA" = "Metolachlor_ethane_sulfonic_acid",
  "Metolachlor_oxanilic_acid_OA" = "Metolachlor_oxanilic_acid",
  "Piperonyl_butoxid" = "Piperonyl_butoxide",
  "Pirimicarb_desmethyl_" = "Pirimicarb_desmethyl",
  "Propyzamide_Pronamide" = "Propyzamide",
  "Propamocarb" = "Propamocarb_hydrochloride",
  "Propyzamide_Pronamide" = "Propyzamide",
  "Pymetrozin" = "Pymetrozine",
  "Spinetoram_J" = "Spinetoram",
  "tau_fluvinate" = "Tau_Fluvalinate"
)
current_ppp_names_cl <- name_map[current_ppp_names]
current_ppp_names_cl <- ifelse(is.na(current_ppp_names_cl),current_ppp_names,current_ppp_names_cl)
current_ppp_names_cl <- c("sample",current_ppp_names_cl)

#check whether name mapping successfull (visual check)
ppp_soil_all_clean_names <- rbind(current_ppp_names_cl,ppp_soil_all)

#assign clean names to soil ppp table
ppp_soil_all_cleaned <- ppp_soil_all
colnames(ppp_soil_all_cleaned)[2:194] <- ppp_soil_all_clean_names[1,2:194]

saveRDS(ppp_soil_all_cleaned,"./01_input/PPP_results_clean_names_2025_02_14.rds")

#clean info names
ppp_info_set_clean <- ppp_info_set %>% 
  mutate(Type = str_replace(Type,"^i","I"),
         Type = str_replace(Type,"\\s*\\(metabolite\\)",""))


#02.1.1 frequency analysis of PPPs overall plots#####
ppp_freq_detected <- ppp_soil_all_cleaned %>% 
  summarise(across(where(is.numeric),~sum(.x>0,na.rm = T)))

ppp_freq_notdetected <- ppp_soil_all_cleaned %>% 
  summarise(across(where(is.numeric),~sum(.x==0,na.rm = T)))

ppp_freq_overview <- ppp_freq_detected %>% 
  bind_rows(ppp_freq_notdetected) %>% 
  mutate(detection_status = c("detected", "not detected"),.before = 1)

#transpose dataframe
#add frequency information
ppp_freq_overview_t <- as.data.frame(t(ppp_freq_overview)) %>% 
  row_to_names(row_number = 1)

ppp_d_plots <- as.numeric(ppp_freq_overview_t$detected)
ppp_nd_plots <- as.numeric(ppp_freq_overview_t$`not detected`)
total_analysed_plots <- ppp_d_plots+ppp_nd_plots
freq_detected_plots <- (ppp_d_plots/total_analysed_plots)*100

ppp_freq_overview_t <- ppp_freq_overview_t %>% 
  mutate(total_analysed_plots = total_analysed_plots) %>% 
  mutate(freq_detected_plots = freq_detected_plots)



write.xlsx(ppp_freq_overview_t,file.path(output_dir,"01_ppp_frequency/ppp_freq_overview_plots.xlsx"))

 
#02.2 frequency analysis per field #####
ppp_freq_detected_perfield <- ppp_soil_all_cleaned %>% 
  group_by(substr(sample,1,3)) %>% 
  summarise(across(where(is.numeric),~sum(.x>0,na.rm = T))) %>% 
  ungroup() %>%
  mutate(detection_status = "detected",.before = 1)


ppp_freq_notdetected_perfield <- ppp_soil_all_cleaned %>% 
  group_by(substr(sample,1,3)) %>% 
  summarise(across(where(is.numeric),~sum(.x==0,na.rm = T))) %>% 
  ungroup() %>% 
  mutate(detection_status = "not detected",.before = 1)


ppp_freq_overview_per_field <- ppp_freq_detected_perfield %>% 
  bind_rows(ppp_freq_notdetected_perfield) %>% 
  rename("field" = "substr(sample, 1, 3)") 

#transform prior to set not_detected to 0 if not containing value 3
ppp_freq_overview_per_field_longer <- pivot_longer(ppp_freq_overview_per_field,
                                                   cols = 3:195,
                                                   names_to = "ppp",values_to = "number")
ppp_freq_overview_per_field_wider_det <- pivot_wider(data = ppp_freq_overview_per_field_longer,
                                                     names_from = detection_status,
                                                     values_from = number)
  
ppp_freq_overview_per_field_wider_det_clean <- ppp_freq_overview_per_field_wider_det %>% 
  mutate('not detected' = if_else(detected > 0, 0,3))

ppp_freq_overview_per_field_clean <- pivot_longer(data = ppp_freq_overview_per_field_wider_det_clean,
                                                  cols = 3:4,
                                                  names_to = "detection_status",
                                                  values_to = "number")

ppp_freq_overview_per_field_clean <- pivot_wider(data = ppp_freq_overview_per_field_clean,
                                                 names_from = ppp,
                                                 values_from = number)

rm(ppp_freq_overview_per_field_wider_det,
   ppp_freq_overview_per_field_longer,
   ppp_freq_overview_per_field_wider_det_clean)

ppp_freq_different_fields <- ppp_freq_overview_per_field_clean %>%
  group_by(detection_status) %>% 
  summarise(across(where(is.numeric), ~sum(.x > 0))) %>% 
  ungroup()

ppp_freq_different_fields_t <- as.data.frame(t(ppp_freq_different_fields)) %>% 
  row_to_names(row_number = 1)

#summarise frequency per field
ppp_d_fields <- as.numeric(ppp_freq_different_fields_t$detected)
ppp_nd_fields <- as.numeric(ppp_freq_different_fields_t$`not detected`)
total_analysed_fields <- ppp_d_fields + ppp_nd_fields
freq_detected_fields <- (ppp_d_fields/total_analysed_fields)*100

ppp_freq_different_fields_t <- ppp_freq_different_fields_t %>% 
  mutate(total_analysed_fields = total_analysed_fields) %>% 
  mutate(freq_detected_fields = freq_detected_fields)


write.xlsx(ppp_freq_different_fields_t,file.path(output_dir,"01_ppp_frequency/ppp_freq_overview_fields.xlsx"))

# ppp_names <- sort(colnames(ppp_soil_all_cleaned))
# write.csv(ppp_names,file.path(output_dir,"01_ppp_frequency/ppp_names.csv"))

#2.3.1 prepare frequency plot and subsective plotting####
ppp_soil_all_cleaned_plus <- ppp_soil_all_cleaned %>% 
  mutate(location = substr(sample,1,1),
         farming_system = substr(sample,3,3),
         plot = substr(sample,5,5),.after=1)

ppp_soil_all_cleaned_long <- ppp_soil_all_cleaned_plus %>% 
  pivot_longer(cols = 5:197,
               names_to = "ppp_compound",
               values_to = "concentrations_ng_g") %>% 
  mutate(detected = if_else(concentrations_ng_g > 0, 1, 0),.after = 4)

#filter out Imidacloprid and Metalaxyl Metabolite as both compounds were detected in blanks
ppp_soil_all_cleaned_long_meta <- ppp_soil_all_cleaned_long %>% 
  left_join(ppp_info_set_clean,by = join_by("ppp_compound" == "PPP_compound_clean_name")) %>% 
  select(-"PPP.compound") %>% 
  filter(ppp_compound != "Imidacloprid",
         ppp_compound != "Metalaxyl_Metabolite")

type_colors <- map_colours(ppp_soil_all_cleaned_long_meta$Type)
type_colors[1] <- "#F05B12FF"
ppp_soil_all_cleaned_long_meta$color <- type_colors[as.character(ppp_soil_all_cleaned_long_meta$Type)]


# Get bar index positions where the first digit changes
vline_positions <- ppp_soil_all_cleaned_long_freq_plot %>%
  distinct(sample) %>%                                  # one per bar
  arrange(sample) %>%
  mutate(location = str_sub(sample, 1, 1),              # first digit
         index = row_number()) %>%
  group_by(location) %>%
  summarise(last_index = max(index), .groups = "drop") %>%
  mutate(vline_pos = last_index + 0.5) %>%
  pull(vline_pos)
# Remove the last one so no line after the final location
vline_positions <- vline_positions[-length(vline_positions)]

ppp_soil_all_cleaned_long_freq_plot <- ppp_soil_all_cleaned_long_meta %>% 
  group_by(sample,Type) %>% 
  summarise(n_detected = sum(detected), .groups = "drop") %>% 
  group_by(Type) %>% 
  filter(sum(n_detected) > 0) %>% 
  ungroup() 
#  filter(n_detected>0)  

#prepare labels to be bold when regenerative sample analysed  
samples <- unique(ppp_soil_all_cleaned_long$sample)

labels <- ifelse(sub("^[0-9]_([0-9]).*", "\\1", samples) == "1",
                 paste0("<b>", samples, "</b>"),
                 samples)


number_of_compounds_plot <- ggplot(ppp_soil_all_cleaned_long_freq_plot,
       aes(x = sample, y = n_detected, fill = Type))+
  geom_bar(stat = "identity",
           color = "black") +
  geom_text(aes(label = ifelse(n_detected == 0, "", n_detected)),
                position = position_stack(vjust = 0.5), size = 3)+
  geom_vline(xintercept = vline_positions,
               linetype = "dashed",
               colour = "black")+
  scale_fill_manual(values = type_colors)+
  scale_x_discrete(labels = labels) +
  labs(title = "Number of Pesticide Compounds Detected in the Investigated Plots",
       x = "Plot ID",
       y = "Number of Compounds")+
  theme_minimal()+
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1),
        legend.position = "bottom")

number_of_compounds_plot
#3.1 prepare cumulative concentration plot and subsective plotting####
ppp_soil_all_cleaned_long_conc_plot <- ppp_soil_all_cleaned_long_meta %>% 
  filter(ppp_compound != "Imidacloprid") %>% 
  group_by(sample,Type) %>%
  summarise(cumulated_concentrations_ng_g = sum(concentrations_ng_g),
            .groups = "drop") %>% 
  group_by(Type) %>% 
  filter(sum(cumulated_concentrations_ng_g)>0) %>% 
  ungroup() %>% 
 # filter(cumulated_concentrations_ng_g>0) %>% 
  mutate(cumulated_concentrations_ng_g = round(cumulated_concentrations_ng_g,1))


total_concentrations <- ppp_soil_all_cleaned_long_meta %>% 
  group_by(sample) %>% 
  summarise(total_concentrations_ng_g = sum(concentrations_ng_g)) %>% 
  mutate(total_concentrations_ng_g = round(total_concentrations_ng_g,1))

  
total_concentrations_plot <- ggplot(ppp_soil_all_cleaned_long_conc_plot,
       aes(x = sample, y = cumulated_concentrations_ng_g, fill = Type))+
  geom_bar(stat = "identity",
           color = "black") +
  geom_text(data = total_concentrations,
            aes(x = sample, y = total_concentrations_ng_g, label = total_concentrations_ng_g),
            inherit.aes = FALSE,
            vjust = 0, hjust = -0.2,
            size = 3, angle=90)+
  geom_vline(xintercept = vline_positions,
             linetype = "dashed",
             colour = "black")+
  labs(title = "Cumulated Pesticide Concentrations Detected in the Investigated Plots",
       x = "Plot ID",
       y = "Concentration [ng/g]")+
  scale_y_continuous(limits = c(0,300))+
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = type_colors)+
  theme_minimal()+
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1),
        legend.position = "bottom")

total_concentrations_plot


#4.1 prepare risk quotient and cumulative risk plotting
#multiplicate pnec (mg/kg) with 1000 in order to come to (ng/g)
pnec_soil_data_clean <- pnec_soil_data %>% 
  mutate(pnec_soil_ng_g = as.numeric(`PNEC_soil.(mg/kg)`)*1000) %>% 
  select(-`PNEC_soil.(mg/kg)`)

ppp_soil_all_cleaned_long_meta_pnec <- ppp_soil_all_cleaned_long_meta %>% 
  left_join(pnec_soil_data_clean,by = join_by(ppp_compound == PPP_compound_clean))

ppp_soil_all_cleaned_long_meta_pnec_rq <- ppp_soil_all_cleaned_long_meta_pnec %>% 
  mutate(risk_quotient = concentrations_ng_g/pnec_soil_ng_g)
str(ppp_soil_all_cleaned_long_meta_pnec_rq)


ppp_soil_all_plots_cumulative_risk <- ppp_soil_all_cleaned_long_meta_pnec_rq %>% 
  group_by(sample,Type) %>% 
  summarise(rq_cumulative = sum(risk_quotient,na.rm = TRUE),.groups = "drop") %>% 
  group_by(Type) %>% 
  filter(sum(rq_cumulative) > 0) %>% 
  ungroup() 

total_risk <- ppp_soil_all_cleaned_long_meta_pnec_rq %>% 
  group_by(sample) %>% 
  summarise(total_risk = sum(risk_quotient, na.rm = TRUE)) %>% 
  mutate(total_risk = round(total_risk,2))

write_rds(total_risk,file = "01_input/ppp_total_risk.rds")

cumulative_risk_plot <- ggplot(ppp_soil_all_plots_cumulative_risk,
                               aes(x = sample, y = rq_cumulative, fill = Type))+
  geom_bar(stat = "identity",
           color = "black") +
  geom_text(data = total_risk,
            aes(x = sample, y = total_risk, label = total_risk),
            inherit.aes = FALSE,
            vjust = 0, hjust = -0.2,
            size = 3, angle=90)+
  geom_hline(aes(yintercept = 1),
             linetype = "dashed",
             color = "red")+
  geom_vline(xintercept = vline_positions,
             linetype = "dashed",
             colour = "black")+
  labs(title = "Cumulated Pesticide Risk Quotient in the Investigated Plots",
       x = "Plot ID",
       y = "Cumulated Risk Quotient")+
  scale_y_continuous(limits = c(0,10))+
  scale_x_discrete(labels = labels) +
  scale_fill_manual(values = type_colors)+
  theme_minimal()+
  theme(axis.text.x = element_markdown(angle = 45, hjust = 1),
        legend.position = "bottom")




cumulative_risk_plot

#4.1 combined ppp plot #####

combined_plot <- number_of_compounds_plot +  
  theme(axis.title.x = element_blank())+
        #axis.text.x  = element_blank()) 
  total_concentrations_plot +
  theme(axis.title.x = element_blank())+
        #axis.text.x  = element_blank()) 
  cumulative_risk_plot +
  plot_layout(ncol = 1, 
              heights = c(1, 1, 1), 
              guides ="collect") +
  plot_annotation(tag_levels = 'a') & 
  theme(legend.position = "bottom")

combined_plot

ggsave(
  file.path(output_dir,"combined_plot_A4.png"),   # or .jpg
  combined_plot,
  width = 19,               # cm (slightly smaller than full A4 to allow margins)
  height = 27,              # cm
  units = "cm",
  dpi = 300                 # 300 dpi = print quality
)

combined_plot

