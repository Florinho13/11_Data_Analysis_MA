#00 Information
#Transformation of texture textfile to Excel
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt

#1. import libraries -----------------------------
library(openxlsx)
library(readr)
library(tidyverse)


#2. transform to .txt to .xlsx ---------------------------------
texture_data <- read.csv("./01_input/Christ_MSc_Texture_all.txt")
write.xlsx(texture_data,"./01_input/christ_msc_texture_all.xlsx")

texture_data <- texture_data

