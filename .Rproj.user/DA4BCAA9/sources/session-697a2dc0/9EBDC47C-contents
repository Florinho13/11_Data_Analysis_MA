#00 Information
#Transformation of texture textfile to Excel
#Code created by Florian Christ as part of the Master Thesis 01.11.2024
#used R Version. 4.4.1 2024-06-14 ucrt

#1. import libraries -----------------------------
library(openxlsx)
library(readr)
library(tidyverse)


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

