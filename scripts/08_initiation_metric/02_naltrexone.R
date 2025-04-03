# -------------------------------------
# Script: 00_naltrexone
# Author: Nick Williams
# Purpose: Identify MOUD naltexrone periods
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

cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

hcpcs <- read_yaml("~/medicaid/low-back-therapies/data/public/hcpcs_codes.yml")$naltrexone

# other services line file
otl <- open_otl()
# pharmacy line file
rxl <- open_rxl()

gap <- 0

# RXL ---------------------------------------------------------------------

rxl <- 
  filter(rxl, NDC == "65757030001") |> 
  select(BENE_ID, 
         NDC,
         CLM_ID,
         NDC_UOM_CD, 
         NDC_QTY,
         DAYS_SUPPLY,
         RX_FILL_DT) |>
  collect()

rxl_nal <- 
  fsubset(rxl, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(moud_end_dt = RX_FILL_DT + days(DAYS_SUPPLY + gap)) |> 
  fselect(BENE_ID, moud_start_dt = RX_FILL_DT, moud_end_dt) |> 
  funique()

# OTL ---------------------------------------------------------------------

# - start with NDC
otl_ndc_nal <- 
  filter(otl, NDC == "65757030001") |> 
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

# - Assuming 30 day supply
otl_ndc_nal <- 
  fsubset(otl_ndc_nal, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(LINE_SRVC_BGN_DT = case_when(
    is.na(LINE_SRVC_BGN_DT) ~ LINE_SRVC_END_DT, 
    TRUE ~ LINE_SRVC_BGN_DT
  )) |> 
  fmutate(moud_end_dt = LINE_SRVC_BGN_DT + gap + 30) |> 
  fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT, moud_end_dt) |> 
  funique()

# - filter down using HCPCS
otl_hcpcs_nal <- 
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

# - Assuming 30 day supply
otl_hcpcs_nal <-
  fsubset(otl_hcpcs_nal, BENE_ID %in% cohort$BENE_ID) |> 
  fmutate(
    LINE_SRVC_BGN_DT = fifelse(is.na(LINE_SRVC_BGN_DT), LINE_SRVC_END_DT, LINE_SRVC_BGN_DT), 
    moud_end_dt = LINE_SRVC_BGN_DT + gap + 30
  ) |> 
  fselect(BENE_ID, moud_start_dt = LINE_SRVC_BGN_DT, moud_end_dt) |> 
  funique()

# combine -----------------------------------------------------------------

nal <- 
  rbindlist(
    list(
      rxl_nal, 
      otl_ndc_nal, 
      otl_hcpcs_nal
    )
  ) |> 
  funique()

# - Save all moud periods for the initial cohort
write_data(nal, "moud_nal_intervals.fst", drv_root)
