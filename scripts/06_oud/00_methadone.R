# -------------------------------------
# Script: 00_methadone.R
# Author: Nick Williams
# Purpose: Identify MOUD methadone periods
# Notes: 
#   - Methadone tablets are considered a 1 day use
#   - 3 week (21 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(collapse)
library(fst)
library(yaml)
library(data.table)

source("~/medicaid/undertreated-pain/R/helpers.R")
save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", save_dir)

otl <- open_otl()

codes <- read_yaml("~/medicaid/undertreated-pain/data/public/hcpcs_codes.yml")$methadone

# - Limit otl to MOUD methadone codes
otl_methadone <- 
  filter(otl, LINE_PRCDR_CD %in% codes) |>
  select(BENE_ID,
         STATE_CD, 
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD) |>
  collect()

# - Limit to those in the initial cohort
# - Calculate the moud end date
otl_methadone <- 
  fsubset(otl_methadone, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(LINE_SRVC_BGN_DT = case_when(
    is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, 
    TRUE ~ LINE_SRVC_BGN_DT
  )) |> 
  fsubset((LINE_PRCDR_CD == "S0109" & STATE_CD == "IA" & year(LINE_SRVC_BGN_DT) == 2016) | 
            LINE_PRCDR_CD != "S0109") |> 
  fmutate(moud_start_dt = LINE_SRVC_BGN_DT, 
          moud_end_dt = moud_start_dt + 21) |> 
  fselect(BENE_ID, moud_start_dt, moud_end_dt)

# - Save all moud periods for the initial cohort
write_data(otl_methadone, "pain_washout_continuous_enrollment_opioid_requirements_moud_methadone_intervals.fst", save_dir)

# - Filter to moud periods where the start or end date is within the washout period
# - If any moud periods are within the washout period, obs is considered as having moud in washout
moud_methadone <- 
  roworder(otl_methadone, BENE_ID, moud_start_dt) |> 
  join(cohort, how = "left") |> 
  fmutate(moud_methadone_washout = int_overlaps(
    interval(moud_start_dt, moud_end_dt),
    interval(washout_start_dt, pain_diagnosis_dt - 1)
  )) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(moud_methadone_washout = as.numeric(sum(moud_methadone_washout) > 0))

# - Rejoin entire initial cohort and save
moud_methadone <- 
  join(cohort, moud_methadone, how = "left") |> 
  fmutate(moud_methadone_washout = replace_na(moud_methadone_washout, 0))

write_data(moud_methadone, "pain_washout_continuous_enrollment_opioid_requirements_moud_methadone_washout.fst", save_dir)
