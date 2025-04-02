# -------------------------------------
# Script: 07_combine_exclusions_exposure_outcome.R
# Author: Nick Williams
# Purpose: Combine exclusion/inclusion criteria, exposure, outcome and censoring files.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)
library(data.table)

source("~/medicaid/undertreated-pain/R/helpers.R")
drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

# base cohort
cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", file.path(drv_root, "exclusion"))
# debse exclusions
debse_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafdebse_exclusions.fst", file.path(drv_root, "exclusion"))
# iph exclusions
iph_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafiph_exclusions.fst", file.path(drv_root, "exclusion"))
# oth exclusions
oth_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_tafoth_exclusions.fst", file.path(drv_root, "exclusion"))
# oud exclusions
oud_exclusions <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst", file.path(drv_root, "exclusion"))
# exposures
exposures <- load_data("exposures_with_subsets.fst", file.path(drv_root, "exposures"))
# censoring
# cens <- load_data("pain_washout_continuous_enrollment_opioid_requirements_censoring.fst", file.path(drv_root, "outcome"))
# # outcomes
# outcomes <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_outcomes.fst", file.path(drv_root, "outcome"))
# hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_outcomes.fst", file.path(drv_root, "outcome"))


cohort <- list(
  cohort, 
  debse_exclusions, 
  iph_exclusions, 
  oth_exclusions, 
  oud_exclusions
) |> 
  reduce(join, how = "left") |>
  mutate(across(everything(), ~ replace_na(., 0)))

# Remove observations with exclusions
cohort <- filter(cohort, if_all(starts_with("exclusion"), \(x) x == 0))

# Add in exposure, outcome, and censoring data
cohort <- 
  join(cohort, exposures, how = "left") 
  # join(outcomes, how = "left") |> 
  # join(hillary, how = "left") |> 
  # join(cens, how = "left") |>
  # mutate(cens_hillary_period_1 = cens_period_1,
  #        cens_hillary_period_2 = cens_period_2,
  #        cens_hillary_period_3 = cens_period_3,
  #        cens_hillary_period_4 = cens_period_4,
  #        cens_hillary_period_5 = cens_period_5
  # )

convert_cens_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau)) {
    modify <- setdiff(cens[match(cens[j], cens):tau], cens[j])
    outcome_j <- outcomes[j]
    DT[get(outcome_j) == 1, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
  }
  DT[]
  DT
}

convert_outcome_to_na <- function (data, outcomes, cens) {
  DT <- as.data.table(data)
  tau <- length(outcomes)
  for (j in 1:(tau - 1)) {
    modify <- outcomes[match(outcomes[j], outcomes):tau]
    cens_j <- cens[j]
    DT[get(cens_j) == 0, `:=`((modify), lapply(.SD, function(x) NA_real_)), .SDcols = modify]
    
    if(j > 1){ # if previously experienced outcome but then censored at later point, considered to have had outcome at subsequent timepoints
      outcome_j_1 <- outcomes[j-1]
      DT[get(outcome_j_1) == 1, `:=`((modify), lapply(.SD, function(x) 1)), .SDcols = modify]
    }
    
    
  }
  DT[]
  DT
}

cohort <- cohort |>
  # convert_outcome_to_na(cohort, paste0("oud_period_", 1:5), paste0("cens_period_", 1:5)) |> 
  # convert_cens_to_na(paste0("oud_period_", 1:5), paste0("cens_period_", 1:5)) |> 
  # convert_outcome_to_na(paste0("oud_hillary_period_", 1:5), paste0("cens_hillary_period_", 1:5)) |> 
  # convert_cens_to_na(paste0("oud_hillary_period_", 1:5), paste0("cens_hillary_period_", 1:5)) |> 
  select(BENE_ID, washout_start_dt, pain_diagnosis_dt,
         starts_with("exposure"), 
         starts_with("subset"))#, 
         # starts_with("cens_period"), 
         # starts_with("cens_hillary_period"), 
         # starts_with("oud_period"),
         # starts_with("oud_hillary_period")) |>
  # mutate(oud_period_5 = case_when(oud_period_4 == 1 ~ 1,
  #                                 cens_period_5 == 0 ~ as.numeric(NA),
  #                                 TRUE ~ oud_period_5),
  #        oud_hillary_period_5 = case_when(oud_hillary_period_4 == 1 ~ 1,
  #                                 cens_hillary_period_5 == 0 ~ as.numeric(NA),
  #                                 TRUE ~ oud_hillary_period_5)
  #        )

write_data(cohort, "inclusion_exclusion_cohort_with_exposure_outcomes.fst", file.path(drv_root, "final"))
