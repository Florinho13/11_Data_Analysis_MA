#contains all functions used in MA data analysis
library(viridis)
library(ggtext)

# 00create date for storing####
today <- format(Sys.Date(), "%Y_%m_%d")

#01 farm colours
fs_colour <- setNames(c("#66c2a5","#fc8d62"),c(1,2))
fs_colour_plus <- setNames(c("#66c2a5","#fc8d62","#3E9BFEFF"),c(1,2,3))

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



#7. Clean soil and plant parameter names function ####
clean_soil_names <- function(df,col_cont_par_name){
  
  parameter_names_clean <- list( "pH" = "pH","Hplus_conc_mol_l" = "Hplus (mol/l)","clay" = "Clay (%)", "silt" = "Silt (%)", "sand" = "Sand (%)",
                                 "microbial_c" = "Microbial C (mg/kg)", "microbial_N" = "Microbial N (mg/kg)", "Ca_aus_kation_mmol/kg" = "CEC Ca (mmol/kg)",
                                 "K_aus_kation_mmol_kg" = "CEC K (mmol/kg)", "Mg_aus_kation_mmol_kg" = "CEC Mg (mmol/kg)",
                                 "Al_aus_kation_mmol_kg" = "CEC Al (mmol/kg)", "Na_aus_kation_mmol_kg" = "CEC Na (mmol/kg)", "Corg" = "Corg (%)",
                                 "cec_NaMgCaKAl_mmol_kg" = "CEC NaMgCaKAl (mmol/kg)","base_saturation" = "Base saturation (%)","N%_normal" = "N (%)", "C%_normal" = "C (%)",
                                 "C/N_normal" = "C/N ratio", "mean_BD_g_cm3" = "Bulk density (g/cm3)")
  
  df_clean_names <- df %>% 
    mutate({{ col_cont_par_name }} := recode({{ col_cont_par_name }}, !!!parameter_names_clean))
  
  
  return(df_clean_names)
}

clean_plant_names <- function(df,col_cont_par_name){
  
  parameter_names_clean <- list("chlorophyll_average" = "Chlorophyll (SPAD)",
                                "height_cm" = "Plant height (cm)",
                                "root_health_score" = "Root health score",
                                "sla" = "Specific Leaf Area (g/cm2)" )
  
  df_clean_names <- df %>% 
    mutate({{ col_cont_par_name }} := recode({{ col_cont_par_name }}, !!!parameter_names_clean))
  
  
  return(df_clean_names)
}

#8. Thesis plot######
thesis_plot <- function(df,column_sample_name,column_measurement_values,fs_mean_lines){
  ggplot(df,aes(x=substr(column_sample_name,1,3),y=column_measurement_values))+
    geom_point(size=2,alpha = 0.7,color = "#2C3E50")+
    geom_hline(
      data = fs_mean_lines,
      aes(yintercept = mean_value, color = farming_system),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    scale_color_manual(values = fs_colour,labels = c("Regenerative","Conventional"))+
    ggtitle("CNS measurements per field")+
    labs(x="Field ID",y="",color ="Farming System Mean:")+
    theme_minimal()+
    theme(
      plot.title = element_text(size = 15,face = "bold",hjust = 0.5),
      axis.title.x = element_text(size=13),
      axis.title.y = element_text(size=13),
      axis.text.x = element_markdown(size = 11,angle = 45,hjust = 1),
      axis.text.y = element_text(size =11),
      strip.text = element_text(size = 13),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 13),
      legend.position = "bottom"
    )
}

#9. scoring function####
#soil health index
score_shi <- function(df) {
  # Identify descriptive and measurement columns
  descriptive_cols <- df[, 1:4]
  measurement_cols <- df[, -(1:4)]
  
  # Define parameters where "less is better"
  less_is_better <- c("mean_BD_g_cm3")
  
  # Apply scoring
  scored <- measurement_cols %>%
    mutate(across(
      .cols = everything(),
      .fns = ~ if (cur_column() %in% less_is_better) {
        min(., na.rm = TRUE) / .
      } else {
        . / max(., na.rm = TRUE)
      }
    )) %>%
    mutate(across(everything(), ~ pmin(., 1)))  # Cap at 1
  
  # Combine descriptive + scored data
  scored_df <- bind_cols(descriptive_cols, scored)
  
  return(scored_df)
}

#plant health index
score_phi <- function(df) {
  # Identify descriptive and measurement columns
  descriptive_cols <- df[, 1:4]
  measurement_cols <- df[, -(1:4)]
  
  # Define parameters where "less is better"
  less_is_better <- c()
  
  # Apply scoring
  scored <- measurement_cols %>%
    mutate(across(
      .cols = everything(),
      .fns = ~ if (cur_column() %in% less_is_better) {
        min(., na.rm = TRUE) / .
      } else {
        . / max(., na.rm = TRUE)
      }
    )) %>%
    mutate(across(everything(), ~ pmin(., 1)))  # Cap at 1
  
  # Combine descriptive + scored data
  scored_df <- bind_cols(descriptive_cols, scored)
  
  return(scored_df)
}


#10. load csv files considering file name for sample name column
#used for leaf area data
load_csv_and_add_filename_to_col <- function(file_path) {
  # Get the base filename
  file_name <- basename(file_path)
  
  # Remove the "_Areas.csv" part
  name_parts <- strsplit(file_name, "_")[[1]]
  
  if (length(name_parts) < 4) return(NULL)  # skip if unexpected format
  
  # Extract parts
  part1 <- name_parts[1]
  part2 <- name_parts[2]
  part3 <- name_parts[3]
  replicate_part <- name_parts[4]
  
  # Remove possible file extension
  replicate_number <- as.numeric(gsub("[^0-9]", "", replicate_part))
  replicate <- as.numeric(substr(replicate_number, 1, 1))  # take only first digit
  
  # Construct sample_name
  sample_name <- paste(part1, part2, part3, sep = "_")
  
  # Read the CSV file
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Add the extracted info
  df <- df %>%
    mutate(
      sample_name = sample_name,
      replicate = replicate,
      source_file = file_name
    )
  
  return(df)
}

#11. spearman helper#####
# ---- helper: safe Spearman cor.test that skips constant/empty cases ----
spearman_safe <- function(x, y) {
  df <- tibble(x = x, y = y) %>% filter(!is.na(x), !is.na(y))
  if (nrow(df) < 3) return(tibble(estimate = NA_real_, p.value = NA_real_, n = nrow(df)))
  # require at least some variability
  if (length(unique(df$x)) < 2 || length(unique(df$y)) < 2)
    return(tibble(estimate = NA_real_, p.value = NA_real_, n = nrow(df)))
  ct <- suppressWarnings(cor.test(df$x, df$y, method = "spearman", exact = FALSE))
  tibble(estimate = unname(ct$estimate), p.value = ct$p.value, n = nrow(df))
}


#12. function to compute all Spearman correlations for a given RA target ----
cor_against_all <- function(target,dat) {
  map_dfr(
    predictor_cols,
    ~ {
      res <- spearman_safe(dat[[target]], dat[[.x]])
      tibble(
        target      = target,
        variable    = .x,
        spearman_r  = res$estimate,
        p_value     = res$p.value,
        n           = res$n
      )
    }
  ) %>%
    mutate(p_adj_BH = p.adjust(p_value, method = "BH")) %>%
    arrange(desc(abs(spearman_r)))
}

#13. lighten ####
lighten <- function(col, amount = 0.85, to = "#FFFFFF") {
  x <- grDevices::col2rgb(col)/255
  y <- grDevices::col2rgb(to)/255
  z <- (1-amount)*x + amount*y
  grDevices::rgb(z[1], z[2], z[3])
}

regen_col <- lighten(fs_colour[1],0.85)
conv_col <- lighten(fs_colour[2],0.85)


#14. thesis table ####


create_thesis_table_ft <- function(input_df){
  input_df %>% 
  flextable() %>% 
  autofit() %>% 
  # ---- Header formatting ----
  bold(part = "header") %>%
  bg(part = "header", bg = "#DDDDDD") %>%
  
  #farm_id
  bold(
    i = ~ str_detect(`Farm ID`, "_1$"),   # condition: ends with "_1"
    j = "Farm ID",                        # only apply to Farm ID column
    bold = TRUE
  ) %>%
  
  border(
    j = "Farm ID", 
    border.right = fp_border(color = "black", width = 1), 
    part = "all"   # applies to header + body
  ) %>% 
  bg(j = "Farm ID", bg = "#F2F2F2", part = "all") %>%
  
  bg(
    i = ~ str_detect(`Farm ID`, "_1$"),   # condition
    bg = regen_col,
    #j = "Farm ID"# all columns will be colored
  ) %>% 
  bg(
    i = ~ str_detect(`Farm ID`, "_2$"),   # condition
    bg = conv_col,                     # all columns will be colored
  ) %>% 
  #export preparation
  set_table_properties(width = 1, layout = "autofit")
}
