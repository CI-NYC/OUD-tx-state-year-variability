# -------------------------------------
# Script: 0x_study_pain_opioids.R
# Author: Nick Williams
# Purpose:
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(yaml)
library(collapse)

source("~/medicaid/undertreated-pain/R/helpers.R")
save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

# Load cohort
cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", save_dir)

# Load opioid pain NDC
opioids <- read_csv("~/medicaid/undertreated-pain/data/public/opioid_pain_ndc.csv")

# Open RX datasets
rxl <- open_rxl()
rxh <- open_rxh()

# extract opioids for pain ------------------------------------------------

opioids <- 
  select(rxl, BENE_ID, CLM_ID, contains("NDC"), DAYS_SUPPLY) |>
  filter(NDC %in% opioids$ndc) |>
  filter(!is.na(BENE_ID)) |>
  inner_join(cohort) |> 
  collect() |> 
  join(opioids, how = "left", on = c("NDC" = "ndc"))
  
prescribers <- 
  rxh |>
  select(BENE_ID, CLM_ID, PRSCRBNG_PRVDR_ID, PRSCRBNG_PRVDR_NPI, DSPNSNG_PRVDR_ID, DSPNSNG_PRVDR_NPI, RX_FILL_DT) |>
  inner_join(cohort) |> 
  collect() |> 
  fsubset(RX_FILL_DT %within% interval(washout_start_dt, exposure_end_dt + 455)) |> 
  fsubset(CLM_ID %in% opioids$CLM_ID)

opioids <- join(opioids, prescribers, how = "right")

no_bup <- fsubset(opioids, drug_name != "Buprenorphine")

# for buprenorphine, only keep strength/day <= 10
bup_only <- 
  fsubset(opioids, drug_name == "Buprenorphine" & dosage_form == "TABLET") |> 
  fmutate(pills_per_day = NDC_QTY / DAYS_SUPPLY, 
          strength_per_day = bup_strength_clean * pills_per_day) |> 
  fsubset(strength_per_day <= 10)

opioids <- 
  fselect(no_bup, BENE_ID, RX_FILL_DT, PRSCRBNG_PRVDR_NPI, DSPNSNG_PRVDR_NPI, DAYS_SUPPLY) |> 
  join(
    fselect(bup_only, BENE_ID, RX_FILL_DT, PRSCRBNG_PRVDR_NPI, DSPNSNG_PRVDR_NPI, DAYS_SUPPLY), 
    how = "full"
  ) |> 
  join(cohort, how = "inner")

write_data(opioids, "pain_washout_continuous_enrollment_opioid_requirements_pain_opioids_dts.fst", save_dir)
