# -------------------------------------
# Script: 00_bup.R
# Author: Nick Williams
# Purpose: Identify MOUD buprenorphine periods
# Notes:
#   - 3 week (21 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(collapse)
library(yaml)

source("~/medicaid/undertreated-pain/R/helpers.R")

save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

# Load cohort
cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", save_dir)

bup_list <- read_fst("~/medicaid/undertreated-pain/data/public/bup_list.fst")
hcpcs <- read_yaml("~/medicaid/undertreated-pain/data/public/hcpcs_codes.yml")$buprenorphine

# other services line file
otl <- open_otl()
# pharmacy line file
rxl <- open_rxl()

# RXL ---------------------------------------------------------------------

rxl <- 
  filter(rxl, NDC %in% bup_list$ndc) |> 
  select(BENE_ID, 
         NDC,
         CLM_ID,
         NDC_UOM_CD, 
         NDC_QTY,
         DAYS_SUPPLY,
         RX_FILL_DT) |>
  collect() 

# - only keep buprenorphine that are used for the treatment of moud
rxl_buprenorphine <- 
  fsubset(rxl, BENE_ID %in% cohort$BENE_ID) |> 
  join(rename(bup_list, NDC = ndc), how = "left") |> 
  fmutate(pills_per_day = NDC_QTY / DAYS_SUPPLY, 
          strength_per_day = strength * pills_per_day) |> 
  fsubset(check == 0 | 
            (check == 1 & strength_per_day >= 10 & strength_per_day < 50)) |> 
  fmutate(moud_end_dt = RX_FILL_DT + days(DAYS_SUPPLY + 21)) |> 
  fselect(BENE_ID, moud_start_dt = RX_FILL_DT, moud_end_dt) |> 
  funique()

# OTL ---------------------------------------------------------------------

# - Limit otl to MOUD buprenorphine codes
# - start with ndc
otl_ndc_bup <- 
  filter(otl, NDC %in% bup_list$ndc) |> 
  select(BENE_ID,
         CLM_ID,
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY) |> 
collect()

otl_ndc_bup <- 
  fsubset(otl_ndc_bup, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(LINE_SRVC_BGN_DT = case_when(
    is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, 
    TRUE ~ LINE_SRVC_BGN_DT
  )) |> 
  join(rename(bup_list, NDC = ndc), how = "left")

otl_ndc_bup <- 
  rbindlist(
    list(
      # - buprenorphine injections have a 30 days supply
      fsubset(otl_ndc_bup, form == "injection") |> 
        fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |> 
        fmutate(moud_end_dt = moud_start_dt + 51), 
      # - BUP-NX, assuming 1 day supply
      fsubset(otl_ndc_bup, form %in% c("tablet","film") & check == 0) |>
        fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |> 
        fmutate(moud_end_dt = moud_start_dt + 21), 
      # - only keep buprenorphine that are used for the treatment of moud
      fsubset(otl_ndc_bup, form %in% c("tablet","film") & check == 1) |> 
        fmutate(strength_times_quantity = fifelse(NDC_UOM_CD == "UN", strength * NDC_QTY, strength)) |> 
        fgroup_by(BENE_ID, LINE_SRVC_BGN_DT) |> 
        fsummarize(strength_per_day = sum(strength_times_quantity)) |> 
        fsubset(strength_per_day >= 10) |> 
        fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |>
        fmutate(moud_end_dt =  moud_start_dt + 21)
    )
  ) |> 
  funique()

# - filter down using HCPCS
otl_hcpcs_bup <- 
  filter(otl, LINE_PRCDR_CD %in% hcpcs) |> 
  select(BENE_ID,
         CLM_ID,
         NDC,
         NDC_UOM_CD, 
         NDC_QTY,
         LINE_SRVC_BGN_DT,
         LINE_SRVC_END_DT,
         LINE_PRCDR_CD,
         LINE_PRCDR_CD_SYS,
         ACTL_SRVC_QTY,
         ALOWD_SRVC_QTY) |> 
  collect()

otl_hcpcs_bup <-
  fsubset(otl_hcpcs_bup, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(
    LINE_SRVC_BGN_DT = fifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT), 
    form = fcase(
      LINE_PRCDR_CD == "J0570", "implant", 
      str_detect(LINE_PRCDR_CD, "Q"), "injection",
      str_detect(LINE_PRCDR_CD, "J"), "tablet"
    )
  ) |> 
  fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT, form) |> 
  fmutate(moud_end_dt = fcase(
    form == "implant", moud_start_dt + 21 + 182,
    form == "injection", moud_start_dt + 21 + 30,
    form == "tablet", moud_start_dt + 21 
  )) |> 
  fselect(-form) |> 
  funique()

# combine  ----------------------------------------------------------------

bup <- 
  rbindlist(
  list(
    rxl_buprenorphine, 
    otl_ndc_bup, 
    otl_hcpcs_bup
  )
) |> 
  funique()

# - Save all moud periods for the initial cohort
write_data(bup, "pain_washout_continuous_enrollment_opioid_requirements_moud_bup_intervals.fst", save_dir)

moud_bup <- 
  roworder(bup, BENE_ID, moud_start_dt) |> 
  join(cohort, how = "left") |> 
  fmutate(moud_bup_washout = int_overlaps(
    interval(moud_start_dt, moud_end_dt),
    interval(washout_start_dt, pain_diagnosis_dt - 1)
  )) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(moud_bup_washout = as.numeric(sum(moud_bup_washout) > 0))

# - Rejoin entire initial cohort and save
moud_bup <- 
  join(cohort, moud_bup, how = "left") |> 
  fmutate(moud_bup_washout = replace_na(moud_bup_washout, 0))

write_data(moud_bup, "pain_washout_continuous_enrollment_opioid_requirements_moud_bup_washout.fst", save_dir)
