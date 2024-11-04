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
                                                          "Heimenhausen","Heimiswil","Ueberstorf"))+ #unique function ensures that every color is selected
  scale_shape_manual(values = c(16,17), labels = c("Regenerative","Conventional"))+
  labs(color = "Location",shape="Farming System")+
  theme_minimal()
  
  return(plot_overview)
}


#4. function to prepare for plotting
plot_prepr <- function(data_to_prepare){
  plot_prepeared <- data_to_prepare %>% 
  mutate(location = substr(sample_name,1,1),
         farming_system = substr(sample_name,3,3),
         plot = substr(sample_name,5,5))
  
  location_colors <- map_colours(plot_prepeared$location)
  
  farming_system_shapes <- map_shapes(plot_prepeared$farming_system)
  
  plot_prepeared$color <- location_colors[as.character(plot_prepeared$location)] 
  plot_prepeared$shape <- farming_system_shapes[as.character(plot_prepeared$farming_system)]
  
  return(plot_prepeared)
}
