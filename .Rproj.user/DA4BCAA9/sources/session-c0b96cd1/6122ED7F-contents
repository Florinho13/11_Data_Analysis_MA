#contains all functions used in MA data analysis


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
