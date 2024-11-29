#00 Information
#Calculation of NDVI per Field on 3 different dates.
#Code created by Florian Christ as part of the Master Thesis 01.11.2024
#used R Version. 4.4.1 2024-06-14 ucrt
install.packages("terra")

#1. import libraries and functions -----------------------------
library(openxlsx)
library(readr)
library(stringr)

library(tidyverse)

#raster data manipulation
library(terra)
library(sf)

source("./03_R/00_functions.R")

#2. import datasets -------------------------
# import all field polygons
gpkg_files <- list.files(path = "./01_input/01_NDVI/03_Field_Polygons", pattern = "*.gpkg", full.names = TRUE)

for(file in gpkg_files){
  field <- st_read(file)
  filename <- tools::file_path_sans_ext(basename(file))
  assign(filename,field)
}


# import all required sentinel 2 data

plot(Field_Code_2_2_1)


#import all sentinel 2 data needed
parent_folder_jp2 <- "../07_QGIS/NDVI/02_Sentinel_Band_4_and_8/01_Months"

#list all jp2 files within parent folder
jp2_files <- list.files(parent_folder_jp2,pattern = "\\.jp2$",recursive = TRUE,
                        full.names = TRUE)

for(file in jp2_files){
  rasters <- rast(file)
  filename <- tools::file_path_sans_ext(basename(file))
  assign(filename,rasters)
}



#3. calculate all the required NDVI values###########
#get a list of the raster object names of each the band 4 and band 8 objects
band4_files <- ls(pattern = "B04")
band8_files <- ls(pattern = "B08")

#combine the two lists in a dataframe
S2_dataframe <- data.frame(band4_files,band8_files)
S2_dataframe <- S2_dataframe %>% 
  mutate(NDVI_name = paste("ndvi_",substr(band4_files,1,22),sep = "")) #add a name for the NDVI rasters.

#ndvi_list <- list() #create an empty list to store the NDVI rasters within.

#calculate NDVI for every date and tile used
for (i in 1:nrow(S2_dataframe)){
  band4_name <- S2_dataframe$band4_files[i]
  band8_name <- S2_dataframe$band8_files[i]
  ndvi_list[[i]] <- calculate_ndvi(band4_name,band8_name)
}

#rename NDVI rasters
names(ndvi_list) <- S2_dataframe$NDVI_name

#store the ndvi rasters as tif
ndvi_folder <- "../07_QGIS/NDVI/03_calculated_ndvi_rasters"

for (name in names(ndvi_list)){
  #retrieve raster from the list
  ndvi_raster <- ndvi_list[[name]]
  
  #Define output file path
  output_file <- file.path(paste0(ndvi_folder,"/",name,".tif"))
  
  #save the raster as a GeoTiff
  writeRaster(ndvi_raster,output_file, overwrite = TRUE)
  
}

field_1_1_1 <- st_transform(field_1_1_1,crs = ndvi_list[["ndvi_T32TMT_20240203T103231"]])

ndvi_crs <- crs(ndvi_list[["ndvi_T32TMT_20240203T103231"]])

field_1_1_1_extent <- ext(field_1_1_1)
field_1_1_1 <- st_transform(Field_Code_1_1_1, crs = 32632)
st_crs(field_1_1_1)
ndvi_cropped <- crop(ndvi_list[["ndvi_T32TMT_20240203T103231"]],field_1_1_1_extent)

plot(ndvi_list[["ndvi_T32TMT_20240203T103231"]],col = terrain.colors(50),ext = field_1_1_1_extent)
plot(st_geometry(field_1_1_1),add = TRUE, lwd = 2)


for (name in S2_dataframe$NDVI_name) {
  plot(ndvi_list[[name]], main = paste("NDVI for", name))
}

