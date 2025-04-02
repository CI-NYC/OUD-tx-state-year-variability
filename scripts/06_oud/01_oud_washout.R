# -------------------------------------
# Script: 01_oud_washout.R
# Author: Nick Williams
# Updated:
# Purpose: Combine OUD expansive definitions to indicate OUD during washout period
# Notes:
# -------------------------------------

library(tidyverse)
library(lubridate)
library(collapse)
library(fst)

source("~/medicaid/undertreated-pain/R/helpers.R")
save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", save_dir)

# load component files ----------------------------------------------------

poison <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_poison_dts.fst", save_dir)
hillary <- load_data("pain_washout_continuous_enrollment_opioid_requirements_oud_hillary_dts.fst", save_dir)
misuse <- load_data("pain_washout_continuous_enrollment_opioid_requirements_washout_oud_misuse.fst", save_dir)
bup <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_bup_washout.fst", save_dir)
methadone <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_methadone_washout.fst", save_dir)
nal <- load_data("pain_washout_continuous_enrollment_opioid_requirements_moud_nal_washout.fst", save_dir)

# combine -----------------------------------------------------------------

# - combine MOUD files
cohort <- 
  list(bup, methadone, nal) |> 
  map(\(x) fselect(x, BENE_ID, 7)) |> 
  reduce(join, how = "left") |> 
  join(cohort, how = "left")

# - add probable misuse
cohort <- 
  fselect(misuse, BENE_ID, exclusion_oud_misuse) |> 
  join(cohort, how = "left")

# - add hillary codes
cohort <- 
  fmutate(hillary, 
        exclusion_oud_hillary = 
          as.numeric(oud_hillary_dt %within% interval(washout_start_dt, pain_diagnosis_dt - 1))) |> 
  fselect(BENE_ID, exclusion_oud_hillary) |> 
  funique() |> 
  join(cohort, how = "right") |> 
  fmutate(exclusion_oud_hillary = replace_na(exclusion_oud_hillary, value = 0))

# - add poison codes
cohort <- 
  fmutate(poison, 
          exclusion_oud_poison = 
            as.numeric(oud_poison_dt %within% interval(washout_start_dt, pain_diagnosis_dt - 1))) |> 
  fselect(BENE_ID, exclusion_oud_poison) |> 
  funique() |> 
  join(cohort, how = "right") |> 
  fmutate(exclusion_oud_poison = replace_na(exclusion_oud_poison, value = 0))

# - combine into one indicator
cohort <- 
  fmutate(cohort, 
          exclusion_oud = 
            as.numeric((exclusion_oud_poison + 
                          exclusion_oud_hillary + 
                          exclusion_oud_misuse + 
                          moud_bup_washout + 
                          moud_methadone_washout + 
                          moud_nal_washout) >= 1)) |> 
  fselect(BENE_ID, exclusion_oud)

# save --------------------------------------------------------------------

write_fst(
  cohort, 
  file.path(save_dir, 
            "pain_washout_continuous_enrollment_opioid_requirements_oud_exclusion.fst")
)
