#script for soil texture analysis
#Code created by Florian Christ as part of the Master Thesis
#used R Version. 4.4.1 2024-06-14 ucrt

#00 set up environment----------------
#import libraries
library(tidyverse)
library(soiltexture)
library(ggplot2)

#source functions
source("./03_R/00_functions.R")

#import data
texture_averaged_clean <- readRDS("./01_input/texture_averaged_clean.rds")

#2. prepare dataset for plotting ##################


#adjust column position to use soiltexture function. Generate possible diferentiation variables.
#dataframe for plot containing every sampling plot
plots_texture_averaged_clean_r <- texture_averaged_clean %>% 
  relocate(sample_name,.after= last_col()) %>% 
  mutate(location = substr(sample_name,1,1),
         farming_system = substr(sample_name,3,3),
         sample_plot = substr(sample_name,5,5)) %>% 
  rename(CLAY = clay,
         SILT = silt,
         SAND = sand)
plots_texture_averaged_clean_r <- as.data.frame(plots_texture_averaged_clean_r)
#dataframe for plot containing the sampling plot as average of a field -> field data

fields_texture_averaged_clean_r <- texture_averaged_clean %>% 
  group_by(sample_name = substr(sample_name,1,3)) %>%
  summarize(CLAY = mean(clay),
            SILT = mean(silt),
            SAND = mean(sand)) %>%
  relocate(sample_name,.after=last_col()) %>%
  mutate(location = substr(sample_name,1,1),
         farming_system = substr(sample_name,3,3),
         sum_texture = CLAY + SILT + SAND)
fields_texture_averaged_clean_r <- as.data.frame(fields_texture_averaged_clean_r)


str(fields_texture_averaged_clean_r)

#3. Plot texture triangles --------------------
#create colour to location link
location_colors <- map_colours(fields_texture_averaged_clean_r$location)

#assign colors to corresponding location
fields_texture_averaged_clean_r$color <- location_colors[as.character(fields_texture_averaged_clean_r$location)]
plots_texture_averaged_clean_r$color <- location_colors[as.character(plots_texture_averaged_clean_r$location)]


#3.1 texture plot for whole fields ######
png("./02_output/01_texture/fields_texture.png", width = 700, height = 700)
TT.plot(
  class.sys = "USDA.TT",
  tri.data = fields_texture_averaged_clean_r,
  col = fields_texture_averaged_clean_r$color,
  pch = ifelse(fields_texture_averaged_clean_r$farming_system=="1",1,6),
  lwd = 2.5,
  cex = 2.2,
  main = "Soil Classification on Analysed fields",
  css.lab= c("[%]Clay 0-2 µm","[%]Silt 2-63µm","[%]Sand 63-2000µm")
)

#add legend for locations
legend("topright",legend = unique(fields_texture_averaged_clean_r$location),
       col = unique(plots_texture_averaged_clean_r$color),
       pch = 16,
       cex = 1.3,
       title = "Locations")

#add legend for farming system
legend(x=82.4,y=76,
       legend = c("Regenerative","Conventional"),
       col = "black",
       pt.lwd = 3,
       cex = 1.2,
       pch = c(1,6),
       title = "Farming System")
dev.off()


#3.2 texture plot for each plot on each field#########
png("./02_output/01_texture/plots_texture.png",width=700,height = 700)
TT.plot(
  class.sys = "USDA.TT",
  tri.data = plots_texture_averaged_clean_r,
  col = plots_texture_averaged_clean_r$color,
  pch = ifelse(plots_texture_averaged_clean_r$farming_system=="1",1,6),
  lwd = 2.5,
  cex = 2.2,
  main = "Soil Classification on each plot analysed on fields",
  css.lab= c("[%]Clay 0-2 µm","[%]Silt 2-63µm","[%]Sand 63-2000µm")
)
#add legend for locations
legend("topright",legend = unique(fields_texture_averaged_clean_r$location),
       col = unique(plots_texture_averaged_clean_r$color),
       pch = 16,
       cex = 1.3,
       title = "Locations")

#add legend for farming system
legend(x=82.4,y=76,
       legend = c("Regenerative","Conventional"),
       col = "black",
       pt.lwd = 3,
       cex = 1.2,
       pch = c(1,6),
       title = "Farming System")
dev.off()

#3.3 Define soil classes#####

