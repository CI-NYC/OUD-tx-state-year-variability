# -------------------------------------
# Script: 00_bup.R
# Author: Nick Williams
# Purpose: Identify MOUD buprenorphine periods
# Notes:
#   - 1 week (7 day) grace period is used
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(collapse)
library(yaml)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

# Load cohort
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

bup_list <- read_fst("~/medicaid/low-back-therapies/data/public/bup_list.fst")
hcpcs <- read_yaml("~/medicaid/low-back-therapies/data/public/hcpcs_codes.yml")$buprenorphine

# other services line file
otl <- open_otl()
# pharmacy line file
rxl <- open_rxl()

gap <- 0

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
  fmutate(moud_end_dt = RX_FILL_DT + days(DAYS_SUPPLY + gap)) |> 
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
        fmutate(moud_end_dt = moud_start_dt + 30 + gap), 
      # - BUP-NX, assuming 1 day supply
      fsubset(otl_ndc_bup, form %in% c("tablet","film") & check == 0) |>
        fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |> 
        fmutate(moud_end_dt = moud_start_dt + gap), 
      # - only keep buprenorphine that are used for the treatment of moud
      fsubset(otl_ndc_bup, form %in% c("tablet","film") & check == 1) |> 
        fmutate(strength_times_quantity = fifelse(NDC_UOM_CD == "UN", strength * NDC_QTY, strength)) |> 
        fgroup_by(BENE_ID, LINE_SRVC_BGN_DT) |> 
        fsummarize(strength_per_day = sum(strength_times_quantity)) |> 
        fsubset(strength_per_day >= 10) |> 
        fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT) |>
        fmutate(moud_end_dt =  moud_start_dt + gap)
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
    form == "implant", moud_start_dt + gap + 182,
    form == "injection", moud_start_dt + gap + 30,
    form == "tablet", moud_start_dt + gap
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
write_data(bup, "moud_bup_intervals.fst", drv_root)
