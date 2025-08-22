#script to check for relation estimation regenerative score and shi/phi optional to other variables
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt

#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggtext)
library(patchwork)
library(flextable)
library(stringr)
library(lme4)
library(lmerTest)
library(lme)
library(purrr)
library(broom)
library(stringr)



#load project functions
source("./03_R/00_functions.R")


#import datasets
soil_biological_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_bio.xlsx")
soil_chemical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_chem.xlsx")
soil_physical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_phys.xlsx")
plant_health_param <- read.csv("./02_output/09_plant_health/combined_plant_data_incl_yiel.csv")[,-c(1,3:8)]

regenerative_score <- read.xlsx("./01_input/regenerative_score_2025_08_16.xlsx",sheet = 1)

plant_health <- read.xlsx("./02_output/09_plant_health/plant_health_total.xlsx")
soil_health <- read.xlsx("./02_output/08_SHI/shi_total_rq.xlsx")



#define output directories
output_dir_reg_score <- "./02_output/13_regenerative_score_tests/"

#01. prepare regenerative score####


#create additional row for farm 2_2 with interpolated ra score
c_names <- colnames(regenerative_score)
additional_row_2_2 <- data.frame("2_2",1,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
colnames(additional_row_2_2) <- c_names

regenerative_score_yr1 <- regenerative_score %>%
  filter(year == 1)

regenerative_score_yr1_conv <- regenerative_score_yr1 %>%
  filter(substr(participant,3,3)== 2)

additional_row_2_2[,12] <- round(mean(regenerative_score_yr1_conv$RA_score),0)

regenerative_score_yr1_ready <- regenerative_score_yr1 %>% 
  bind_rows(additional_row_2_2) %>% 
  select(participant,RA_score)

#get four year average ra scores and add average conventional to 2_2
regenerative_score_mean <- regenerative_score %>% 
  group_by(participant) %>%
  summarise(RA_average = mean(RA_score),.groups = "drop")

regenerative_score_mean_conv <- regenerative_score_mean %>% 
  filter(substr(participant,3,3)== 2)

c_names <- colnames(regenerative_score_mean)
additional_row_2_2 <- data.frame("2_2",NA)
colnames(additional_row_2_2) <- c_names

additional_row_2_2[,2] <- mean(regenerative_score_mean_conv$RA_average)

regenerative_score_mean_ready <- regenerative_score_mean %>% 
  bind_rows(additional_row_2_2)

regenerative_score_yr1_average <- regenerative_score_yr1_ready %>% 
  left_join(regenerative_score_mean_ready)

rm(regenerative_score_mean,regenerative_score_mean_conv,regenerative_score_yr1,
   regenerative_score_yr1_conv)
  
write.xlsx(regenerative_score_yr1_average,file.path(output_dir_reg_score,"total_ra_scores_per_field.xlsx"))

#02. simple correlations
shapiro.test(soil_chemical_param$pH) #phi_total 0.01736 no normal distr, shi_total = 0.288 normal distr

#combine datasets
combined_data_soil <- soil_biological_param %>% 
  left_join(soil_chemical_param,by="sample_name") %>% 
  left_join(soil_physical_param,by="sample_name") %>% 
  left_join(soil_health[,c(1,13)],by="sample_name")

combined_data_plant <- plant_health_param %>%
  left_join(plant_health[,c(1,10,11)],by="sample_name")

combined_plant_soil <- combined_data_soil %>% 
  left_join(combined_data_plant,by = "sample_name")


combined_plant_soil_mean <- combined_plant_soil %>% 
  group_by(substr(sample_name,1,3)) %>% 
  summarise(across(2:30,~mean(.x,na.rm = TRUE))) %>% 
  rename("field_id" = "substr(sample_name, 1, 3)")

combined_plant_soil_ra <- combined_plant_soil_mean %>% 
  left_join(regenerative_score_yr1_average, by = join_by("field_id"=="participant"))

write.xlsx(combined_plant_soil_ra,file.path(output_dir_reg_score,"combined_plant_soil_ra.xlsx"))


#calculate spearman
# ---- columns to exclude from correlation as predictors ----
id_cols    <- c("field_id")          # add more IDs if you have them
score_cols <- c("RA_score", "RA_average")

num_cols <- combined_plant_soil_ra %>%
  select(where(is.numeric)) %>%
  names()
predictor_cols <- setdiff(num_cols, c(id_cols, score_cols))




# ---- run for both RA_score and RA_average ----
cor_RA_score   <- cor_against_all("RA_score",dat = combined_plant_soil_ra)
cor_RA_average <- cor_against_all("RA_average",dat = combined_plant_soil_ra)

# Combined tidy table
cor_long <- bind_rows(cor_RA_score, cor_RA_average)

write.xlsx(cor_long,file.path(output_dir_reg_score,"correlation_means_with_RA_scores.xlsx"))
# ---- inspect top results ----
head(cor_RA_score, 10)
head(cor_RA_average, 10)

# ---- (optional) write results to CSV ----
write.csv(cor_RA_score,   "spearman_RA_score.csv",   row.names = FALSE)
write.csv(cor_RA_average, "spearman_RA_average.csv", row.names = FALSE)
write.csv(cor_long,       "spearman_all_results.csv", row.names = FALSE)

# ---- (optional) quick comparison plot: top |rho| per target ----
top_k <- 10
plot_dat <- cor_long %>%
  mutate(sig_raw = p_value < 0.05) %>% 
  group_by(target) %>%
  slice_max(order_by = abs(spearman_r), n = top_k, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(variable = str_trunc(variable, 30)) %>%
  arrange(target, spearman_r) %>%
  mutate(variable = factor(variable, levels = unique(variable)))

ggplot(plot_dat, aes(x = variable, y = spearman_r, fill=sig_raw)) +
  geom_col() +
  facet_wrap(~ target, scales = "free_y") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(x = NULL, y = "Spearman's rho", title = "Spearman correlations with RA metrics") +
  theme_minimal(base_size = 12)

# ---- (optional) correlation matrix of only RA variables vs predictors ----
# This is a compact matrix view (Spearman), useful for heatmaps
mat_vars <- c(score_cols, predictor_cols)
cmat <- cor(combined_plant_soil_ra[, mat_vars], use = "pairwise.complete.obs", method = "spearman")
cmat_RA <- cmat[score_cols, predictor_cols, drop = FALSE]
cmat_RA[ , 1:min(10, ncol(cmat_RA))]  # view first 10 columns
