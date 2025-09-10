#script to check for statistical differences between regenerative and conventional
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt
# ---- Regenerative vs Conventional: paired tests within location (and plot) ----
#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggtext)
library(broom.mixed)
library(patchwork)
library(flextable)
library(officer)
library(broom)
library(purrr)
library(stringr)
library(lme4)
library(nlme)
library(lmerTest)
library(janitor)


#load project functions
source("./03_R/00_functions.R")


soil_biological_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_bio.xlsx")
soil_chemical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_chem.xlsx")
soil_physical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_phys.xlsx")
plant_health_param <- read.csv("./02_output/09_plant_health/combined_plant_data_incl_yiel.csv")[,-c(1,3:8)]
soil_risk <- readRDS("./02_output/11_ppp_comparison/ppp_all_cleaned_long_meta_rq.rds")




--- Packages ---
  pkgs <- c("tidyverse","readxl","janitor","lme4","lmerTest","broom.mixed")
new  <- setdiff(pkgs, rownames(installed.packages()))
if(length(new)) install.packages(new, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 1) Parse code and go long ---
to_long <- function(df){
  message("too long wird ausgeführt.")
  df <- clean_names(df)
  stopifnot("sample_name" %in% names(df))
  meta <- tibble(
    sample_name = as.character(df$sample_name),
    location    = substr(df$sample_name, 1, 1),
    system_code = substr(df$sample_name, 3, 3),
    plot        = substr(df$sample_name, 5, 5),
    field_id    = substr(df$sample_name,1,3)
  ) %>%
    mutate(system = case_when(
      system_code == "1" ~ "Regenerative",
      system_code == "2" ~ "Conventional",
      TRUE ~ NA_character_
    )) %>%
    mutate(
      location = factor(location),
      system   = factor(system, levels = c("Conventional","Regenerative")),
      field_id = factor(field_id),
      plot     = factor(plot)
    ) %>%
    select(-system_code)
  
  # numeric measurement columns only
  num_cols <- df %>% select(-sample_name) %>% select(where(is.numeric)) %>% names()
  
  df %>%
    select(all_of(c("sample_name", num_cols))) %>%
    right_join(meta, by = "sample_name") %>%
    pivot_longer(cols = all_of(num_cols), names_to = "variable", values_to = "value") %>%
    filter(!is.na(system), !is.na(location))
}

# --- 2) Fit one LMM per variable: value ~ system + (1|location) ---
fit_one_field_in_loc <- function(long_df, var){
  dat <- long_df %>% dplyr::filter(variable == var, is.finite(value))
  has_two_sys <- dplyr::n_distinct(dat$system) == 2
  n_loc <- dplyr::n_distinct(dat$location)
  n_obs <- nrow(dat)
  
  mr <- mean(dat$value[dat$system=="Regenerative"], na.rm=TRUE)
  mc <- mean(dat$value[dat$system=="Conventional"], na.rm=TRUE)
  if(!has_two_sys || n_loc < 2 || n_obs < 4){
    return(tibble::tibble(
      variable = var, n_obs = n_obs, n_locations = n_loc,
      beta_systemRegenerative = NA_real_, se = NA_real_, df = NA_real_,
      t = NA_real_, p_val = NA_real_,
      wilcox_W = NA_real_, wilcox_p = NA_real_,
      mean_regen = mr, mean_conv = mc, mean_diff = mr - mc
    ))
  }
  
  #mixed model
  fit <- tryCatch(
    
    nlme:: lme(fixed = value~system,
               random = ~ 1 | location/field_id,
               data = dat,
               method = "REML",
               na.action = na.omit,
               control = nlme::lmeControl(msMaxIter = 200,msMaxEval = 200)
               ),
    error = function(e) NULL
    # lmer(value ~ system + (1|field_id), data = dat, REML = TRUE), # version only considering field as effect
    # #lmer(value ~ system + + (1 + system | location)), #additionally considers the plot variations not possible due to limited data
    # error = function(e) NULL
  )
  
  # extract fixed effect for "systemRegenerative"
  if (is.null(fit)) {
    est <- se <- df <- tval <- pvl <- NA_real_
    AIC_REML <- BIC_REML <- logLik_REML <- AIC_ML <- NA_real_
  } else {
    tt <- tryCatch(summary(fit)$tTable, error = function(e) NULL)
    if (is.null(tt)) {
      est <- se <- df <- tval <- pvl <- NA_real_
    } else {
      rn  <- rownames(tt)
      idx <- which(rn == "systemRegenerative")
      if (length(idx) == 1) {
        est  <- as.numeric(tt[idx, "Value"])
        se   <- as.numeric(tt[idx, "Std.Error"])
        df   <- as.numeric(tt[idx, "DF"])
        tval <- as.numeric(tt[idx, "t-value"])
        pvl  <- as.numeric(tt[idx, "p-value"])
      } else {
        est <- se <- df <- tval <- pvl <- NA_real_
      }
    }
    
    AIC_REML    <- tryCatch(AIC(fit),    error = function(e) NA_real_)
    
    qqfile <- tryCatch(
      save_qqplot_lme(fit, variable = var,
                      model_label = "value ~ system + (1 | location)"),
      error = function(e) NA_character_
    )
    
    # tt <- broom.mixed::tidy(fit, effects = "fixed", conf.int = FALSE) %>%
    #   dplyr::filter(term == "systemRegenerative")
    # est <- dplyr::coalesce(tt$estimate, NA_real_)
    # se  <- dplyr::coalesce(tt$std.error, NA_real_)
    # df  <- dplyr::coalesce(tt$df, NA_real_)
    # tval<- dplyr::coalesce(tt$statistic, NA_real_)
    # pvl <- dplyr::coalesce(tt$p.value, NA_real_)
    }
  
  
  # if (is.null(fit) || isTRUE(lme4::isSingular(fit))) {
  #   mr <- mean(dat$value[dat$system=="Regenerative"], na.rm=TRUE)
  #   mc <- mean(dat$value[dat$system=="Conventional"], na.rm=TRUE)
  #   return(tibble::tibble(
  #     variable = var, n_obs = n_obs, n_locations = n_loc,
  #     beta_systemRegenerative = NA_real_, se = NA_real_, df = NA_real_,
  #     t = NA_real_, p_val = NA_real_,
  #     mean_regen = mr, mean_conv = mc, mean_diff = mr - mc
  #   ))
  # # }
  # 
  # # Safe pulls: if a column is absent, set NA_real_
  # est <- dplyr::coalesce(tt$estimate, NA_real_)
  # se  <- dplyr::coalesce(tt$std.error, NA_real_)
  # df  <- dplyr::coalesce(tt$df, NA_real_)
  # tval<- dplyr::coalesce(tt$statistic, NA_real_)
  # pvl <- dplyr::coalesce(tt$p.value, NA_real_)
  
  # --- Unpaired Wilcoxon test ---
    df_fields <- dat %>%
      dplyr::group_by(system, field_id) %>%
      dplyr::summarise(field_mean = mean(value, na.rm = TRUE), .groups = "drop")
  
  
    wil <- tryCatch(
      wilcox.test(field_mean ~ system, data = df_fields, exact = TRUE),
      error = function(e) NULL
    )
    if (is.null(wil)) {
      Wstat <- NA_real_; Wp <- NA_real_
    } else {
      Wstat <- unname(wil$statistic)
      Wp <- wil$p.value
    }
  
  # --- Shapiro-Wilk normality test ---
    shap <- tryCatch(
      shapiro.test(dat$value),
      error = function(e) NULL
    )
    if (is.null(shap)) {
      shap_W <- NA_real_; shap_p <- NA_real_
    } else {
      shap_W <- shap$statistic
      shap_p <- shap$p.value
    }
  
    tibble::tibble(
      variable = var,
      n_obs = n_obs,
      n_locations = n_loc,
      beta_systemRegenerative = est,
      se = se,
      df = df,
      t = tval,
      p_val = pvl,                          # <— renamed
      AIC_REML = AIC_REML,
      wilcox_W = Wstat,            # Wilcoxon W statistic
      wilcox_p = Wp,
      shap_w = shap_W,
      shap_p = shap_p,
      mean_regen = mr,
      mean_conv  = mc,
      mean_diff  = mr - mc
    )
  }

fit_all_vars_field_in_loc <- function(df, domain_label){
  long_df <- to_long(df)
  vars <- long_df %>% dplyr::distinct(variable) %>% dplyr::pull(variable)
  res <- purrr::map_dfr(vars, ~fit_one_field_in_loc(long_df, .x)) %>%
    dplyr::mutate(
      domain = domain_label,
      direction = dplyr::case_when(
        is.finite(beta_systemRegenerative) & beta_systemRegenerative > 0 ~ "Regenerative > Conventional",
        is.finite(beta_systemRegenerative) & beta_systemRegenerative < 0 ~ "Regenerative < Conventional",
        TRUE ~ "NA"
      )
    ) %>%
    dplyr::relocate(domain, variable) %>%
    dplyr::arrange(.data$p_val) %>%                # <— use column explicitly
    dplyr::mutate(p_adj_fdr = p.adjust(p_val, method = "fdr"))
  res
}

fit_all_vars_field_in_loc_qq <- function(df, domain_label, qq_out_dir = "./qqplots_overall"){
  long_df <- to_long(df)
  vars <- long_df %>% dplyr::distinct(variable) %>% dplyr::pull(variable)
  purrr::map_dfr(vars, ~fit_one_field_in_loc(long_df, .x))%>%
    # ... mutate/arrange/etc ...
    dplyr::mutate(p_adj_fdr = p.adjust(p_val, method = "fdr"))
}

# --- 3) Run for your three files (or swap in your in-memory data frames) ---
bio_df  <- readxl::read_xlsx("/mnt/data/soil_comb_bio.xlsx")
chem_df <- readxl::read_xlsx("/mnt/data/soil_comb_chem.xlsx")
phys_df <- readxl::read_xlsx("/mnt/data/soil_comb_phys.xlsx")

bio_res_field_in_loc  <- fit_all_vars_field_in_loc_qq(soil_biological_param,  "biological")
chem_res_field_in_loc <- fit_all_vars_field_in_loc_qq(soil_chemical_param, "chemical")
phys_res_field_in_loc <- fit_all_vars_field_in_loc_qq(soil_physical_param, "physical")



all_res_field_in_loc <- bind_rows(bio_res_field_in_loc, chem_res_field_in_loc, phys_res_field_in_loc) %>% 
  mutate(model = "field_in_loc_as_random", .after = 2)
#only valid after running 17, 17_2
all_soil_res_models <- bind_rows(all_res_compl,all_res_field,all_res_field_in_loc)
write.xlsx(all_soil_res_models,"./02_output/12_statistical_tests/soil_mixed_model_tests.xlsx")


plant_res_field_in_loc <- fit_all_vars_field_in_loc(plant_health_param, "plant") %>% 
  mutate(model = "field_in_loc_as_random", .after = 2)

all_plant_res_models <- bind_rows(plant_res_compl,plant_res_field,plant_res_field_in_loc)
write.xlsx(all_plant_res_models,"./02_output/12_statistical_tests/plant_mixed_model_tests.xlsx")

phi_res_field_in_loc <- fit_all_vars_field_in_loc(phi_total[,-c(2:4)],"phi") %>% 
  mutate(model = "field_in_loc_as_random", .after = 2)


all_phi_res_models <- bind_rows(phi_res_compl, phi_res_field, phi_res_field_in_loc)
write.xlsx(all_phi_res_models,"./02_output/12_statistical_tests/phi_mixed_model_tests.xlsx")

shi_res_field_in_loc <- fit_all_vars_field_in_loc(scored_soil_data_rq_total[,-c(2:4)],"shi") %>% 
  mutate(model = "field_in_loc_as_random", .after = 2)

write.xlsx(all_shi_res_model,"./02_output/12_statistical_tests/shi_mixed_model_tests.xlsx")


all_shi_res_model <- bind_rows(shi_res_compl,shi_res_field,shi_res_field_in_loc)

write.csv(all_res,"./02_output/12_statistical_tests/mixed_model_tests.csv")
write.csv(plant_res,"./02_output/12_statistical_tests/mixed_model_plant_tests.csv")

readr::write_csv(bio_res,  "lmm_results_biological.csv")
readr::write_csv(chem_res, "lmm_results_chemical.csv")
readr::write_csv(phys_res, "lmm_results_physical.csv")
readr::write_csv(all_res,  "lmm_results_all.csv")

#3.1 prepare ppp stats
soil_ppp_summary <- soil_risk %>% 
  group_by(sample) %>% 
  summarise(cumulative_RQ = sum(risk_quotient,na.rm = TRUE),
            total_conc = sum(concentrations_ng_g,na.rm = TRUE),
            number_of_subst = sum(detected, na.rm = TRUE),
            .groups = "drop") %>%
  rename("sample_name" = "sample")

soil_ppp_summary_long <- to_long(soil_ppp_summary)

soil_ppp_summary_overview <- soil_ppp_summary_long %>% 
  group_by(system,variable) %>% 
  summarise(average = mean(value,na.rm =TRUE),
            med = median(value,na.rm =TRUE),
            standev= sd(value,na.rm = TRUE),
            minimal_value = min(value, na.rm = TRUE),
            maximal_value = max(value,na.rm = TRUE))


soil_ppp_summary_type <- soil_risk %>% 
  group_by(sample, Type) %>% 
  summarise(cumulative_RQ = sum(risk_quotient,na.rm = TRUE),
            total_conc = sum(concentrations_ng_g,na.rm = TRUE),
            number_of_subst = sum(detected, na.rm = TRUE),
            .groups = "drop") %>%
  rename("sample_name" = "sample")


soil_ppp_summary_overview_type <- soil_ppp_summary_type %>% 
  group_by(substr(sample_name,3,3),Type) %>% 
  summarise(sum_per_type = sum(cumulative_RQ))

ppp_accumulated_risk_per_subst <- soil_risk %>% 
  group_by(ppp_compound) %>% 
  summarise(sum(risk_quotient))

ppp_res_field_in_loc <- fit_all_vars_field_in_loc(soil_ppp_summary,"Pesticide")
write.xlsx(ppp_res,"./02_output/12_statistical_tests/pesticides_field_in_loc_mixed_model_tests.xlsx")

# --- 4) Quick glance: strongest effects (raw p) ---
all_res %>%
  arrange(p_val, desc(abs(beta_systemRegenerative))) %>%
  select(domain, variable, n_locations, n_obs,
         beta_systemRegenerative, se, df, t, p, p_adj_fdr, mean_diff, direction) %>%
  head(20)




# Convert to long format if not already
# (assuming you have soil data frames prepared)
df_long <- to_long(soil_biological_param) %>%
  bind_rows(to_long(soil_chemical_param)) %>%
  bind_rows(to_long(soil_physical_param))

# Open a PDF to save all plots
pdf("mixed_model_residual_diagnostics.pdf", width = 8, height = 6)

for (v in unique(df_long$variable)) {
  dat <- df_long %>% filter(variable == v, is.finite(value))
  
  # Only run if both systems are present in >=2 locations
  if (n_distinct(dat$system) == 2 && n_distinct(dat$location) >= 2) {
    fit <- try(lmer(value ~ system + (1|location), data = dat, REML = TRUE), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      par(mfrow = c(1,3))  # 3 plots side by side
      
      # 1. Residuals vs fitted
      plot(fit, main = paste("Residuals vs Fitted:", v))
      
      # 2. QQ-plot
      qqnorm(residuals(fit), main = paste("QQ Plot:", v))
      qqline(residuals(fit), col = "red")
      
      # 3. Histogram
      hist(residuals(fit), main = paste("Histogram:", v), xlab = "Residuals")
    }
  }
}

dev.off()



#3. create thesis tables####
#biological means table
soil_biological_param_mean_field <- soil_biological_param %>%
  group_by(substr(sample_name,1,3)) %>% 
  summarise(across(2:3,~round(mean(.x,na.rm = TRUE),2)))%>% 
  rename("Farm ID" = "substr(sample_name, 1, 3)",
         "Microbial C (mg/g)" = "microbial_c",
         "Microbial N (mg/g)" = "microbial_N")

soil_bio_table <- create_thesis_table_ft(soil_biological_param_mean_field)
soil_bio_table

doc <- read_docx() %>% 
  body_add_par("Table X: Plant health parameters by field", style = "heading 1") %>% 
  body_add_flextable(
    soil_bio_table %>% 
      autofit() %>% 
      set_table_properties(width = 1, layout = "autofit")   # scales table to page width
  ) %>%
  body_add_par("", style = "Normal")

print(doc,target = "./02_output/14_thesis_tables/thesis_table_soil_bio.docx")


#physical_means_table
soil_physical_param_mean_field <- soil_physical_param %>% 
  group_by(substr(sample_name,1,3)) %>% 
  summarise(across(2:5,~round(mean(.x,na.rm = TRUE),2)))%>% 
  rename("Farm ID" = "substr(sample_name, 1, 3)",
         "Clay (%)" = "clay",
         "Silt (%)" = "silt",
         "Sand (%)" = "sand",
         "Bulk Density (g/cm3)" = "mean_BD_g_cm3") %>% 
  mutate(Classification = c("SiLo","SiLo","SaLo","SiLo","SiLo","SiLo","SiLo",
                            "SiLo","SiLo","Lo","Lo"),.after=1)




soil_phys_table <- create_thesis_table_ft(soil_physical_param_mean_field)
soil_phys_table
doc <- read_docx() %>% 
  body_add_par("Table X: Plant health parameters by field", style = "heading 1") %>% 
  body_add_flextable(
    soil_phys_table %>% 
      autofit() %>% 
      set_table_properties(width = 1, layout = "autofit")   # scales table to page width
  ) %>%
  body_add_par("", style = "Normal")

print(doc,target = "./02_output/14_thesis_tables/thesis_table_soil_phys.docx")


#chemical_means_table
soil_chem_param_mean_field <- soil_chemical_param %>% 
  select(sample_name,Hplus_conc_mol_l,cec_NaMgCaKAl_mmol_kg,base_saturation,`C%_normal`,
         Corg,`N%_normal`,`C/N_normal`) %>% 
  group_by(substr(sample_name,1,3)) %>% 
  summarise(across(2:8,~mean(.x,na.rm = TRUE))) %>% 
  rename('Farm ID' = "substr(sample_name, 1, 3)",
         "pH" = "Hplus_conc_mol_l",
         "CEC NA,Mg,Ca,K,Al (mmol/kg)" = "cec_NaMgCaKAl_mmol_kg",
         "Base Saturation" = "base_saturation",
         "C (%)" = "C%_normal",
         "Corg (%)" = "Corg",
         "N (%)" = "N%_normal",
         "C/N Ratio" = "C/N_normal") %>% 
  mutate(pH = -log10(pH)) %>% 
  mutate(across(2:8,~round(.x,2)))

soil_chem_table <- create_thesis_table_ft(soil_chem_param_mean_field)
soil_chem_table


doc <- read_docx() %>% 
  body_add_par("Table X: Plant health parameters by field", style = "heading 1") %>% 
  body_add_flextable(
    soil_chem_table %>% 
      autofit() %>% 
      set_table_properties(width = 1, layout = "autofit")   # scales table to page width
  ) %>%
  body_add_par("", style = "Normal")

print(doc,target = "./02_output/14_thesis_tables/thesis_table_soil_chem.docx")


#residual qqplot
safe_name <- function(x) str_replace_all(x, "[^A-Za-z0-9_]+", "_")

save_qqplot_lme <- function(fit, variable, model_label,
                            out_dir = "./02_output/12_statistical_tests/01_residual_qq_field_in_loc",
                            width = 5, height = 5, dpi = 300) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  # normalized residuals from nlme::lme
  res <- resid(fit, type = "normalized")
  dd  <- data.frame(resid = as.numeric(res))
  
  p <- ggplot(dd, aes(sample = resid)) +
    stat_qq() + stat_qq_line() +
    labs(title = paste0("QQ-plot: ", variable),
         subtitle = model_label,
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_minimal(base_size = 12)
  
  path <- file.path(out_dir, paste0(safe_name(variable), "_qq.png"))
  ggsave(path, plot = p, width = width, height = height, dpi = dpi)
  path
}
