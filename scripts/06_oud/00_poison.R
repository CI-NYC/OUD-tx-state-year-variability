# -------------------------------------
# Script: 00_poison.R
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

source("~/medicaid/undertreated-pain/R/helpers.R")
save_dir <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort/exclusion"

cohort <- load_data("pain_washout_continuous_enrollment_opioid_requirements.fst", save_dir)

# Source ICD codes
codes <- read_yaml("~/medicaid/undertreated-pain/data/public/oud_codes.yml")$cochran_poison

# Read in IPH dataset
iph <- open_iph()

# read in OTH 
oth <- open_oth()

iph_poison <- iph |>
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  collect() |> 
  inner_join(cohort) |> 
  mutate(SRVC_BGN_DT = case_when(
    is.na(SRVC_BGN_DT) ~ SRVC_END_DT, 
    TRUE ~ SRVC_BGN_DT)
  ) |> 
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> 
  drop_na(cd) |> 
  mutate(oud_poison_dt = case_when(cd %in% codes ~ SRVC_BGN_DT)) |>  
  drop_na(oud_poison_dt) |> 
  distinct(BENE_ID, oud_poison_dt)

oth_poison <- oth |> 
  select(BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  inner_join(cohort) |> 
  collect() |> 
  filter(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  # define oud variable of interest via codes
  mutate(oud_poison_dt = case_when(DGNS_CD_1 %in% codes ~ SRVC_BGN_DT,
                                   DGNS_CD_2 %in% codes ~ SRVC_BGN_DT)) |>
  drop_na(oud_poison_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_poison_dt)

oud_poison <- 
  bind_rows(iph_poison, oth_poison) |> 
  distinct()

oud_poison <- 
  inner_join(oud_poison, cohort) |> 
  filter(oud_poison_dt %within% interval(washout_start_dt, exposure_end_dt + 455))

write_data(oud_poison, "pain_washout_continuous_enrollment_opioid_requirements_oud_poison_dts.fst", save_dir)
