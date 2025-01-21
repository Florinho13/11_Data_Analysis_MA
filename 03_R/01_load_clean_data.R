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


#KAK
#results KAK Ca
KAK_Ca <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="KAK_results_Ca")
#results KAK NaMgAlKMn
KAK_NaMgAlKMn <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="KAK_results_NaMgAlKMn")
#blanks
KAK_blanks <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="blanks")
#element properties
KAK_elements <- read.xlsx("./01_input/2024_09_20_KAK_all_Florian.xlsx",sheet="elements")


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
  filter(!str_starts(sample_name,"Blank"))

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

KAK_NaMgAlKMn_clean_plus_ordered_selected <- KAK_NaMgAlKMn_clean_plus_ordered %>% 
  select(2,3,6:25)

KAK_combined <- KAK_Ca_clean %>% 
  left_join(KAK_NaMgAlKMn_clean_plus_ordered_selected,by = c("sample" = "sample", "replicate" = "replicate"),keep = FALSE)

write.xlsx(KAK_combined,"./02_output/KAK_all_excel_2024_09_20.xlsx")
saveRDS(KAK_combined,"./01_input/KAK_all_2024_09_20.rds")
