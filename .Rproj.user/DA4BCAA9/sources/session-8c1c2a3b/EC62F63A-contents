#00 Information
#Transformation of texture textfile to Excel
#Code created by Florian Christ as part of the Master Thesis 01.11.2024
#used R Version. 4.4.1 2024-06-14 ucrt

#1. import libraries -----------------------------
library(openxlsx)
library(readr)
library(tidyverse)
library(janitor)


#2. import datasets ---------------------------------
#create date for storing
today <- format(Sys.Date(), "%Y_%m_%d")

#texture data
texture_data <- read.csv("./01_input/Christ_MSc_Texture_all.txt")
#write.xlsx(texture_data,"./01_input/christ_msc_texture_all.xlsx")

#pH
pH_data <- read.xlsx("./01_input/lab_resultate_bern.xlsx",sheet = 1)


#microbial biomass
#microbial C
microbial_C <- read.xlsx("./01_input/Data_microbial_biomass_BT_Janina_Hämmerli.xlsx", sheet = "microbial_c")
#microbial N
microbial_N <- read.xlsx("./01_input/Data_microbial_biomass_BT_Janina_Hämmerli.xlsx", sheet = "microbial_n")


#plant health
#Plant height
plant_height <- read.xlsx("./01_input/Plant_Health.xlsx",sheet="höhe")
#Chlorophyll content
plant_chlorophyll <- read.xlsx("./01_input/Plant_Health.xlsx",sheet = "chlorophyll")

#bulk density
#bulk density
bulk_density <- read.xlsx("./01_input/BulkDensity_MA_final.xlsx",sheet = "BD")


#CNS measurement data
CNS_data <- read.xlsx("./01_input/MaChr_Florian_resultsCNS_2024_12_03.xlsx", sheet = "CNS")


#KAK
#results KAK Ca
KAK_Ca <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="KAK_results_Ca")
#results KAK NaMgAlKMn
KAK_NaMgAlKMn <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="KAK_results_NaMgAlKMn")
#blanks
KAK_blanks <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="blanks")
#element properties
KAK_elements <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="elements")

#root health
root_health_data <- read.xlsx("./01_input/results_root_health_assessment_2025_01_10.xlsx",sheet = "root_health")

#results PPP soil
PPP_results_positive <- read.xlsx("./01_input/ppp_results_florian_final_2025_02_14.xlsx",sheet = "positive")
PPP_results_negative <- read.xlsx("./01_input/ppp_results_florian_final_2025_02_14.xlsx",sheet = "negative")
PPP_results_gly_ampa <- read.xlsx("./01_input/ppp_results_florian_final_2025_02_14.xlsx",sheet = "Gly_AMPA")
PPP_results_gc <- read.xlsx("./01_input/ppp_results_florian_final_2025_02_14.xlsx",sheet = "GC")
#import weigh of analysed soil samples
PPP_soil_weighin <- read.xlsx("./01_input/ppp_soil_weighin_florian_2025_01_20.xlsx",sheet = "weigh_in_PPP_analysis")

#3. clean texture_data#####
#Simplify sample name and set column data types
texture_data <- texture_data %>%
  mutate(Sample.Name = str_replace(Sample.Name,"Z",""),
         Sample.Name = str_replace(Sample.Name," - ","_"),
         Sample.Name = as.character(Sample.Name),
         across(10:44,as.numeric))

#create data frame only containing average values of each MS 2000 measurement
texture_data_important <- texture_data %>% 
  select(Record.number,Sample.Name,X0.02μm.2μm,X2μm.63μm,X63μm.2000μm) %>% 
  filter(str_ends(Sample.Name,"_Average"))

#create data frame that averages the averages of the measurement dupli or triplicates
texture_data_important_averaged <- texture_data_important %>% 
  group_by(sample_name = substr(Sample.Name,1,5)) %>% 
  summarize(clay = mean(X0.02μm.2μm),
            silt = mean(X2μm.63μm),
            sand = mean(X63μm.2000μm))

saveRDS(texture_data_important_averaged,"./01_input/texture_averaged_clean.rds")

#4. clean pH Data#######
pH_data_clean <- pH_data %>% 
  select(-c(2,4:7)) %>% 
  rename(sample_name = Sample) %>%
  mutate(sample_name = str_replace(sample_name,"Z","")) %>% 
  filter(!str_starts(sample_name,"Blank")) %>% 
  mutate(Hplus_conc_mol_l = 10^(-(pH)))

saveRDS(pH_data_clean,"./01_input/pH_data_clean.rds")

#5. clean microbial Data####
#microbial C
microbial_C_clean <- microbial_C %>% 
  rename(sample_name = Field_Plot,
         microbial_c = C.probe.calc) %>% 
  mutate(sample_name = str_replace_all(sample_name,"\\.","_"))

saveRDS(microbial_C_clean,"./01_input/microbialC_data_clean.rds")

#microbial N
microbial_N_clean <- microbial_N %>% 
  rename(sample_name = Field_Plot,
         microbial_N = N.probe.calc) %>% 
  mutate(sample_name = str_replace_all(sample_name,"\\.","_"))

saveRDS(microbial_N_clean,"./01_input/microbialN_data_clean.rds")

#6. clean plant data####
#plant height
plant_height_clean <- plant_height %>% 
  rename(sample_name = Feldcode,
         height_cm = "Höhe.[cm]")


saveRDS(plant_height_clean,"./01_input/plant_height_data_clean.rds")

#plant chlorophyll content
plant_chlorophyll_clean <- plant_chlorophyll %>% 
  rename(sample_name = Feldcode,
         chlorophyll_mid = Chlorophyllgehalt.mitte,
         chlorophyll_front = Chlorophyllgehalt.vorne) %>% 
  select(1:4)

saveRDS(plant_chlorophyll_clean,"./01_input/plant_chlorophyll_data_clean.rds")


plant_chlorophyll_clean_long <- plant_chlorophyll_clean %>%
  pivot_longer(
    cols = c(chlorophyll_mid, chlorophyll_front),
    names_to = "chlorophyll_type",
    values_to = "chlorophyll_value"
  )

saveRDS(plant_chlorophyll_clean_long,"./01_input/plant_chlorophyll_data_clean_long.rds")



#7. clean KAK data####
#KAK Ca
KAK_Ca_clean <- KAK_Ca %>% 
  select(1:12) %>% 
  row_to_names(row_number = 1) %>% 
  slice(-n())

#KAK NaMgAlKMn
KAK_NaMgAlKMn_clean <- KAK_NaMgAlKMn %>% 
  row_to_names(row_number = 1) %>% 
  mutate_at(vars(4:15),as.numeric)

#calculate the CEC as well as the cation concentration per kg for each measured element
vec_elements <- KAK_elements$element[2:6]

#generate dataframe to store calculated data
KAK_NaMgAlKMn_clean_plus <- KAK_NaMgAlKMn_clean


#calculate columns containing the Element content in mg/kg as well as the cation exchange capacity
#a for loop is used for this purpose iterating through all the elements
for (elem in vec_elements){
  # store elements ammount of valence electrons
  e_Ladung <- KAK_elements$Ladung[KAK_elements$element == elem]
  
  # store elements molar mass
  molare_masse <- KAK_elements$`MM_mg/mM`[KAK_elements$element == elem]
  
  # store average blank value for element
  blank_average <- KAK_blanks$bl_average[KAK_blanks$element == elem]
  
  #new column name for cation weight per kg soil
  coln_mg_kg <- paste(elem,"conc_mg_kg",sep = "_")
  
  #new column name for CEC
  coln_CEC <- paste(elem,"aus_kation_mmol_kg", sep = "_")
  
  #object used to select column
  colselecter <- paste(elem,"Conc_ug",sep = "_")
  
  #store column containing all measured concentrations in the soil samples
  conc_column_ug_l <- KAK_NaMgAlKMn_clean %>% 
    select(starts_with(colselecter)) %>% 
    pull()
  
  #calculate column containing cation weight per kilo soil
  KAK_NaMgAlKMn_clean_plus <- KAK_NaMgAlKMn_clean_plus %>% 
    mutate(!!coln_mg_kg := ((((conc_column_ug_l*20)-(blank_average*20))*(100/1000)/(KAK_NaMgAlKMn_clean$einwaage_g/1000))/1000))
  #calculate column containing the cation exchange capacity
  KAK_NaMgAlKMn_clean_plus <- KAK_NaMgAlKMn_clean_plus %>% 
    mutate(!!coln_CEC := (!!sym(coln_mg_kg) /molare_masse)*e_Ladung)
  
}

#replace all values below 0 with 0
KAK_NaMgAlKMn_clean_plus <- KAK_NaMgAlKMn_clean_plus %>% 
  mutate(across(where(is.numeric),~ifelse(.<0,0,.)))

#order values alphabetical in the columns
KAK_NaMgAlKMn_clean_plus_ordered <- KAK_NaMgAlKMn_clean_plus %>% 
  select(1:5,sort(names(KAK_NaMgAlKMn_clean_plus)[6:25]))

KAK_NaMgAlKMn_clean_plus_ordered_selected <- KAK_NaMgAlKMn_clean_plus_ordered %>% 
  select(2,3,6:25)

KAK_combined <- KAK_Ca_clean %>% 
  left_join(KAK_NaMgAlKMn_clean_plus_ordered_selected,by = c("sample" = "sample", "replicate" = "replicate"),keep = FALSE) %>% 
  rename("sample_name" = "sample") %>% 
  mutate_at(vars(6:32),as.double)

KAK_combined_soilcomb <- KAK_combined %>% 
  select(-c(1,4,5,6,7,10,8,9,15,16,19,20,23,24,27,28,31,32)) %>% 
  mutate(cec_NaMgCaKAl_mmol_kg = rowSums(select(.,4,5,7,9,13))) %>%
  mutate(base_saturation = (rowSums(select(.,4,7,9,13))/cec_NaMgCaKAl_mmol_kg)*100) %>% 
  group_by(sample_name) %>% 
  summarise(across(where(is.numeric),~ mean(.x,na.rm = TRUE)))

#create date for storing
today <- format(Sys.Date(), "%Y_%m_%d")

write.xlsx(KAK_combined,paste("./02_output/03_KAK/KAK_all_excel_",today,".xlsx",sep = ""))
saveRDS(KAK_combined,paste("./01_input/KAK_all_.rds",sep = ""))

#8. clean Bulk Density data#####
bulk_density_clean <- bulk_density[1:64,1:10] %>% 
  mutate("replicate" = str_sub(sample,7,7),.after = 1) %>% 
  mutate("sample_name" = str_sub(sample,1,5),.after = 1) %>% 
  mutate(Put.in.the.oven = convertToDateTime(Put.in.the.oven)) %>% 
  mutate(weighing.day = convertToDateTime(weighing.day)) %>% 
  rename("BD_g_cm3"="BD_g_cm3)") %>% 
  select(-sample)

saveRDS(bulk_density_clean,paste("./01_input/bulk_density_clean",today,".rds",sep = ""))

bulk_density_clean_soilcomb <- bulk_density_clean %>% 
  select(-c(3:8,10,11)) %>% 
  group_by(sample_name) %>% 
  summarise(mean_BD_g_cm3 = mean(BD_g_cm3, na.rm = TRUE))

#9. clean CNS measurement data#####
CNS_data_clean <- CNS_data %>% 
  mutate(Name = str_replace(Name,"Z",""),
         Name = str_replace(Name,"\\.",""),
         C_fraction = substr(Name,8,8),.after = Name, 
         C_fraction = if_else(C_fraction == "b","normal","muffled"),
         replicate = substr(Name,7,7),
         Name = substr(Name,1,5)) %>% 
  rename("sample_name"= "Name")



saveRDS(CNS_data_clean,"./01_input/CNS_data_clean.rds")

#9.1 Calculate Corg, and averages of the replicates ############
#get dataframe for normal samples
CNS_data_normal <- CNS_data_clean %>% 
  filter(C_fraction == "normal") %>% 
  select(sample_name,replicate,'N%','C%', '%S','C/N')

#get dataframe for muffled samples
CNS_data_muffled <- CNS_data_clean %>% 
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

CNS_data_normal_muffled_mean_Corg_soilcomb <- CNS_data_normal_muffled_mean_Corg %>% 
  select(-starts_with(c("%S","N%_m")))
#10. combine datasets #####
#combine all the datasets to one offering an overview
soil_data_combined <- pH_data_clean %>% 
  left_join(texture_data_important_averaged,by="sample_name") %>%  
  left_join(microbial_C_clean,by="sample_name") %>% 
  left_join(microbial_N_clean,by="sample_name") %>% 
  left_join(KAK_combined_soilcomb,by="sample_name") %>% 
  left_join(CNS_data_normal_muffled_mean_Corg_soilcomb,by="sample_name") %>% 
  left_join(bulk_density_clean_soilcomb,by = "sample_name")

saveRDS(soil_data_combined,"./01_input/soil_data_combined.rds")  
write.xlsx(soil_data_combined,paste("./01_input/soil_data_combined_",today,".xlsx",sep = ""))





#11. root health####
root_health_data_clean <- root_health_data %>% 
  rename("sample_name" = "Sample_Code", "root_health_score" = "Root_Health_Score") %>% 
  select(-Check)

saveRDS(root_health_data_clean,paste("./01_input/rooth_health_clean",today,".rds",sep = ""))

#12. clean PPP soil data####
#positive
PPP_results_positive_cl <- PPP_results_positive %>% 
  mutate(Sample = str_replace(Sample,"_P","")) %>% 
  rename("sample" = "Sample")
  
#recalculate the values from ng/ml (solvent) to ng/g (soil)
#add column with weight of the analysed soil samples.
#positive
PPP_results_positive_weight <- PPP_results_positive_cl %>% 
  left_join(PPP_soil_weighin[c(1,6)],by="sample")

PPP_results_positive_clean_ng_g <- PPP_results_positive_weight[5:36,1:150] %>% 
  mutate(across(everything(),~ifelse(.=="<LOQ",0,.))) %>%
  mutate_at(vars(2:150),as.numeric) %>% 
  mutate(across(2:149,~if_else(is.na(`weigh_que_corr.[g]`),.x,.x*15/`weigh_que_corr.[g]`))) %>% 
  select(-150)


rm(PPP_results_positive_cl,PPP_results_positive_weight)  

#negative

PPP_results_negative_cl <- PPP_results_negative[6:37,1:16] %>% 
  mutate(Sample = str_replace(Sample,"_P_neg","")) %>% 
  rename("sample" = "Sample")

PPP_results_negative_weight <- PPP_results_negative_cl %>% 
  left_join(PPP_soil_weighin[c(1,6)], by = "sample")

PPP_results_negative_clean_ng_g <- PPP_results_negative_weight %>%
  mutate(across(everything(), ~ifelse(.=="<LOQ",0,.))) %>% 
  mutate_at(vars(2:17),as.numeric) %>% 
  mutate(across(2:17,~if_else(is.na(`weigh_que_corr.[g]`),.x,.x*15/`weigh_que_corr.[g]`))) %>% 
  select(-17)

rm(PPP_results_negative_cl,PPP_results_negative_weight)

#gly/ampa
PPP_results_gly_ampa_cl <- PPP_results_gly_ampa[6:37,1:3] %>% 
  mutate(Sample = str_replace(Sample,"_G","")) %>% 
  rename("sample" = "Sample")

PPP_results_gly_ampa_clean_ng_g <- PPP_results_gly_ampa_cl %>%
  mutate(across(everything(), ~ifelse(.=="<10",0,.)))

rm(PPP_results_gly_ampa_cl)


#GC
PPP_results_gc_cl <- PPP_results_gc %>% 
  select(c(1,5:36)) %>% 
  pivot_longer(-'ng/g',names_to = "sample", values_to = "value") %>% 
  pivot_wider(names_from='ng/g',values_from = "value") %>% 
  mutate(sample = str_replace(sample,"_MR",""))

PPP_results_gc_clean_ng_g <- PPP_results_gc_cl %>%
  mutate(across(2:29, ~ifelse(grepl("^&lt",.),0,as.numeric(.))))

rm(PPP_results_gc_cl)

#combine
PPP_results_combined <- PPP_results_gly_ampa_clean_ng_g %>% 
  left_join(PPP_results_positive_clean_ng_g,by="sample") %>% 
  left_join(PPP_results_negative_clean_ng_g,by="sample") %>% 
  left_join(PPP_results_gc_clean_ng_g,by="sample")

saveRDS(PPP_results_combined,"./01_input/PPP_results_clean_2025_02_14.rds")
write.xlsx(PPP_results_combined,"./01_input/PPP_results_clean_2025_02_14.xlsx")
