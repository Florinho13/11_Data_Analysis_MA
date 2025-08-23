#script to check for statistical differences between regenerative and conventional
#Code created by Florian Christ as part of the Master Thesis 02.08.2025
#used R Version. 4.4.1 2024-06-14 ucrt
# ---- Regenerative vs Conventional: paired tests within location (and plot) ----
#00. Setup_Environment######
#import libraries
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(ggttext)
library(broom.mixed)
library(patchwork)
library(flextable)
library(officer)
library(broom)
library(purr)
library(stringr)
library(lme4)
library(lmerTest)
library(lme)
library(janitor)


#load project functions
source("./03_R/00_functions.R")


soil_biological_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_bio.xlsx")
soil_chemical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_chem.xlsx")
soil_physical_param <- read.xlsx("./02_output/12_statistical_tests/soil_comb_phys.xlsx")
plant_health_param <- read.csv("./02_output/09_plant_health/combined_plant_data_incl_yiel.csv")[,-c(1,3:8)]




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
    plot        = substr(df$sample_name, 5, 5)
  ) %>%
    mutate(system = case_when(
      system_code == "1" ~ "Regenerative",
      system_code == "2" ~ "Conventional",
      TRUE ~ NA_character_
    )) %>%
    mutate(
      location = factor(location),
      system   = factor(system, levels = c("Conventional","Regenerative")),
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
fit_one <- function(long_df, var){
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
    lmer(value ~ system + (1|location), data = dat, REML = TRUE), # version only considering location as effect
    #lmer(value ~ system + + (1 + system | location)), #additionally considers the plot variations not possible due to limited data
    error = function(e) NULL
  )
  
  if (is.null(fit) || isTRUE(lme4::isSingular(fit))) {
    est <- se <- df <- tval <- pvl <- NA_real_
  } else {
    tt <- broom.mixed::tidy(fit, effects = "fixed", conf.int = FALSE) %>%
      dplyr::filter(term == "systemRegenerative")
    est <- dplyr::coalesce(tt$estimate, NA_real_)
    se  <- dplyr::coalesce(tt$std.error, NA_real_)
    df  <- dplyr::coalesce(tt$df, NA_real_)
    tval<- dplyr::coalesce(tt$statistic, NA_real_)
    pvl <- dplyr::coalesce(tt$p.value, NA_real_)
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
  wil <- tryCatch(
    wilcox.test(value ~ system, data = dat, exact = FALSE),
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
    wilcox_W = Wstat,            # Wilcoxon W statistic
    wilcox_p = Wp,
    shap_w = shap_W,
    shap_p = shap_p,
    mean_regen = mr,
    mean_conv  = mc,
    mean_diff  = mr - mc
  )
}

fit_all_vars <- function(df, domain_label){
  long_df <- to_long(df)
  vars <- long_df %>% dplyr::distinct(variable) %>% dplyr::pull(variable)
  res <- purrr::map_dfr(vars, ~fit_one(long_df, .x)) %>%
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

# --- 3) Run for your three files (or swap in your in-memory data frames) ---
bio_df  <- readxl::read_xlsx("/mnt/data/soil_comb_bio.xlsx")
chem_df <- readxl::read_xlsx("/mnt/data/soil_comb_chem.xlsx")
phys_df <- readxl::read_xlsx("/mnt/data/soil_comb_phys.xlsx")

bio_res  <- fit_all_vars(soil_biological_param,  "biological")
chem_res <- fit_all_vars(soil_chemical_param, "chemical")
phys_res <- fit_all_vars(soil_physical_param, "physical")

plant_res <- fit_all_vars(plant_health_param, "plant")

all_res <- bind_rows(bio_res, chem_res, phys_res)

phi_res <- fit_all_vars(plant_health,"phi")
shi_res <- fit_all_vars(soil_health,"shi")

write.csv(all_res,"./02_output/12_statistical_tests/mixed_model_tests.csv")
write.csv(plant_res,"./02_output/12_statistical_tests/mixed_model_plant_tests.csv")

readr::write_csv(bio_res,  "lmm_results_biological.csv")
readr::write_csv(chem_res, "lmm_results_chemical.csv")
readr::write_csv(phys_res, "lmm_results_physical.csv")
readr::write_csv(all_res,  "lmm_results_all.csv")

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
