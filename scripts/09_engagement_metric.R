# -------------------------------------
# Script: 08_mediator_physical_therapy.R
# Author: Nick Williams
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for physical therapy
#   during the mediator period.
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)
library(tidyr)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

# Read in cohort and dates
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root) 

codes <- read_yaml("~/medicaid/low-back-therapies/data/public/oud_codes.yml")$hillary


# Computing the date of MOUD initation -----------------------------------------
bup <- load_data("moud_bup_intervals.fst", drv_root)
methadone <- load_data("moud_methadone_intervals.fst", drv_root)
naltrexone <- load_data("moud_nal_intervals.fst", drv_root)

moud <- rbind(bup, methadone, naltrexone)

setDT(moud)
setkey(moud, BENE_ID)

moud <- moud[, .(BENE_ID, moud_start_dt, moud_end_dt)] |>
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |>
  distinct()

moud <- moud |>
  left_join(cohort |> select(BENE_ID, index_dt)) |>
  filter(moud_start_dt %within% interval(index_dt, index_dt + days(14)))

moud <- moud[, .SD[moud_start_dt == min(moud_start_dt)], by = BENE_ID] |>
  select(BENE_ID, moud_start_dt) |>
  distinct()


# Finding subsequent OUD diagnosis codes ---------------------------------------

# Read in IPH dataset
iph <- open_iph()

# Read in OTH 
oth <- open_oth()

iph_engagement <-
  select(iph, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  right_join(moud, by="BENE_ID") |>
  collect() |>
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  filter(SRVC_BGN_DT %within% interval(moud_start_dt + days(1), moud_start_dt + days(30))) |>
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> 
  drop_na(cd) |> 
  mutate(oud_hillary_dt = case_when(cd %in% codes ~ SRVC_BGN_DT)) |>  
  drop_na(oud_hillary_dt) |> 
  distinct(BENE_ID, oud_hillary_dt)

oth_engagement <-
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  right_join(moud, by="BENE_ID") |>
  collect() |> 
  filter(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  filter(SRVC_BGN_DT %within% interval(moud_start_dt + days(1), moud_start_dt + days(30))) |>
  # define oud variable of interest via codes
  mutate(oud_hillary_dt = case_when(DGNS_CD_1 %in% codes ~ SRVC_BGN_DT,
                                    DGNS_CD_2 %in% codes ~ SRVC_BGN_DT)) |>
  drop_na(oud_hillary_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_hillary_dt)

engagement <- 
  bind_rows(iph_engagement, oth_engagement) |> 
  # distinct() |>
  group_by(BENE_ID) |>
  summarise(engagement_metric = as.integer(n() >= 2))

cohort <- cohort |>
  left_join(engagement) |>
  mutate(engagement_metric = ifelse(is.na(engagement_metric), 0, engagement_metric)) |>
  select(BENE_ID, engagement_metric)
  

write_data(cohort, "engagement_metric.fst", drv_root)
