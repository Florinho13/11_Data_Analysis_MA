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
