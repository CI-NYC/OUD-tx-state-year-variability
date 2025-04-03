# -------------------------------------
# Script: 00_methadone.R
# Author: Nick Williams
# Purpose: Identify MOUD methadone periods
# Notes: 
#   - Methadone tablets are considered a 1 day use
#   - 1 week (7 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(collapse)
library(fst)
library(yaml)
library(data.table)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

otl <- open_otl()

codes <- read_yaml("~/medicaid/low-back-therapies/data/public/hcpcs_codes.yml")$methadone

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
          moud_end_dt = moud_start_dt + 7) |> 
  fselect(BENE_ID, moud_start_dt, moud_end_dt)

# - Save all moud periods for the initial cohort
write_data(otl_methadone, "moud_methadone_intervals.fst", drv_root)
