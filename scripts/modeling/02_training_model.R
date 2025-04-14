
library(tictoc)
library(tidyverse)
library(lubridate)
library(mlr3)
library(mlr3learners)
library(mlr3extralearners) 
library(mlr3superlearner)
library(purrr)
library(glue)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")
set.seed(72)

save_dir <- "/mnt/general-data/disability/post_surgery_opioid_use/tmp"

outcomes <- c("initiation_metric",
              "engagement_metric",
              "retention_metric",
              "counseling_metric")


learners <- c("mean", "glm", "xgboost", "earth", "ranger", "lasso") 

# cohort <- load_data("hillary_cohort_with_exclusions_noncontinuous_enrollment.fst", file.path(drv_root, "modelling"))
cohort <- load_data("hillary_cohort_clean_imputed.fst", file.path(drv_root, "modelling"))


cohort <- cohort |>
  left_join(initiation_metric) |>
  left_join(engagement_metric) |>
  left_join(retention_metric) |>
  left_join(counseling_metric) |>
  mutate(across(c("C_14","C_44","C_180"), ~ replace_na(., 0)))

# columns:
# BENE_ID
# 3 OUD columns for inclusion filtering - has OUD before... Dec17, Nov17, Jul4
  # exclusion_..._washout
# 3 censoring columns - remained enrolled for... 14/44/180 days
  # exclusion_..._contenroll, rename to C_14/44/180
# 4 outcome columns - initation, engagement, retention, counseling

# steps
# 1. |> filter(washout)
# 2. |> filter(censor) (only for training model)
# 3. for i in 1:4, fit model on target outcome using W



fits <- list()
analysis_cohort <- cohort |>
  filter(disability_pain_cal == pain)


for (outcome in outcomes){
  
  # nrare = sum(analysis_cohort[,treatments[i]])
  # neff = min(nrow(analysis_cohort), 5*nrare)
  # cv_folds <- ifelse(neff >= 10000, 2, 5)

  fit <- mlr3superlearner(data = analysis_cohort[remained_enrolled=1, c("age_enrollment",
                                                     "SEX_M",
                                                     "race_multi_na",
                                                     "race_black",
                                                     "race_hispanic",
                                                     "race_aian_hpi",
                                                     "race_asian",
                                                     .... # add in more
                                                     outcome,
                                                     )],
                          
                          discrete = F,
                          target = outcome,
                          library = learners,
                          outcome_type = "binomial",
                          folds = cv_folds)
  fit <- list(fit = fit, metric = outcome)
  fits <- append(fits, list(fit))
}

saveRDS(fits, file.path(drv_root, "mlr3_results.rds"))


