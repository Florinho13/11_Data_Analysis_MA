#script to get evaluation of ppp measurements in soil
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt


#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(hrbrthemes)



#load project functions
source("./03_R/00_functions.R")


#load data
ppp_soil_all <- readRDS("./01_input/PPP_results_clean_2025_02_14.rds")
ppp_metadata_sprint_coding <- read.xlsx("./01_input/Metadata_All_CSS_EHA_and_PPP_coding_Aug_2023.xlsx",sheet = 5)
pnec_soil_data <- read.xlsx("./01_input/PNEC_soil_water_list_120722.xlsx", sheet = 2)[,c(1,23)]

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
  "Metalaxyl_Metabolite_CGA_62826_87764_37_2" = "Metalaxyl_M",
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



