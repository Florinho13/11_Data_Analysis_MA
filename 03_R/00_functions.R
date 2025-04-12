#contains all functions used in MA data analysis
library(viridis)

#1. function to map colours####
map_colours <- function(variables_to_map){
  #create color palette
  nr_of_colours <- length(unique(variables_to_map))
  viridis_colors <- turbo(nr_of_colours)
  
  #map palette to variables
  unique_variables <- sort(unique(variables_to_map))
  location_colors <- setNames(viridis_colors, unique_variables)
  
  return(location_colors)
}


#2. function to map shapes#####
map_shapes <- function(farming_sys_variable){
  shape_types <- c(16,17)
  
  unique_variables <- sort(unique(farming_sys_variable))
  variable_shapes <- setNames(shape_types,unique_variables)
  
  return(variable_shapes)
}


#3. function overview plot####
overview_plot <- function(plot_dataset,sample_name,y_value,fs_shape,location,plot_theme,x_axis,y_axis,locolor){
  plot_overview <- ggplot(plot_dataset,aes(x=substr(sample_name,1,3),y=y_value,pch=factor(fs_shape),colour = factor(location)))+
  geom_point(stat = "identity", position = "identity")+
  ggtitle(paste(plot_theme," values on the different field plots\
          and field average"))+
  labs(y=y_axis,x=x_axis)+
  stat_summary(
    fun = mean, 
    geom = "point",
    aes(shape = "Mean Value"),
    shape = 8,
    color = "black",  # Color of the mean line (you can change this as needed)
    size = 2,) +# Thickness of the line
  scale_color_manual(values = locolor , labels = c("Freudwil","Niederhasli","Läufelfingen",
                                                          "Heimenhausen","Heimiswil","Ueberstorf"))+
  scale_shape_manual(values = c(16,17), labels = c("Regenerative","Conventional"))+
  labs(color = "Location",shape="Farming System")+
  theme_minimal()
  
  return(plot_overview)
}


#4. function to prepare for plotting ######
plot_prepr <- function(data_to_prepare){
  plot_prepeared <- data_to_prepare %>% 
  mutate(location = substr(sample_name,1,1),
         farming_system = substr(sample_name,3,3),
         plot = substr(sample_name,5,5),.after=1)
  
  location_colors <- map_colours(plot_prepeared$location)
  
  farming_system_shapes <- map_shapes(plot_prepeared$farming_system)
  
  plot_prepeared$color <- location_colors[as.character(plot_prepeared$location)] 
  plot_prepeared$shape <- farming_system_shapes[as.character(plot_prepeared$farming_system)]
  
  return(plot_prepeared)
}




#5. NDVI calculation function #####
calculate_ndvi <- function(b4_name, b8_name) {
  b4 <- get(b4_name)    #using the get function name corresponding raster data is accessed.
  b8 <- get(b8_name)
  (b8 - b4) / (b8 + b4) #calculate NDVI
}


#6. Adjust Ellipsoid to the one used in Sentinel 2 EPSG 32632
polygon_trans_to_32632 <- function(polygon_data_to_transform,name){
  #name for transformed polygon
  polygon_name <- paste("field", substr(name,12,16),sep = "_")
  
  #transform polygon
  transformed_polygon <- st_transform(polygon_data_to_transform, crs = 32632)
  
  assign(polygon_name,transformed_polygon, envir = .GlobalEnv)
}



#7. Clean soil parameter names function ####
clean_soil_names <- function(current_parameter_names,df,name_of_parameter_column){
  
  parameter_names_clean <- list( "pH" = "pH","Hplus_conc_mol_l" = "Hplus (mol/l)","clay" = "Clay (%)", "silt" = "Silt (%)", "sand" = "Sand (%)",
                                 "microbial_c" = "Microbial C (mg/kg)", "microbial_N" = "Microbial N (mg/kg)", "Ca_aus_kation_mmol/kg" = "CEC Ca (mmol/kg)",
                                 "K_aus_kation_mmol_kg" = "CEC K (mmol/kg)", "Mg_aus_kation_mmol_kg" = "CEC Mg (mmol/kg)",
                                 "Al_aus_kation_mmol_kg" = "CEC Al (mmol/kg)", "Na_aus_kation_mmol_kg" = "CEC Na (mmol/kg)", "Corg" = "Corg (%)",
                                 "cec_NaMgCaKAl_mmol_kg" = "CEC NaMgCaKAl (mmol/kg)","base_saturation" = "Base saturation (%)","N%_normal" = "N (%)", "C%_normal" = "C (%)",
                                 "C/N_normal" = "C/N ratio", "mean_BD_g_cm3" = "Bulk density (g/cm3)")
  
  df_clean_names <- df %>% 
    mutate({{ name_of_parameter_column }} := recode({{ name_of_parameter_column }}, !!!parameter_names_clean))
  
  
  return(df_clean_names)
}


