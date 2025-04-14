# -------------------------------------
# Script: 00_hillary.R
# Author: Nick Williams
# Purpose: Identify OUD using Hillary codes
# Notes:
# -------------------------------------

library(arrow)
library(tidyverse)
library(lubridate)
library(data.table)
library(fst)
library(yaml)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

# Source ICD codes
codes <- read_yaml("~/medicaid/low-back-therapies/data/public/oud_codes.yml")$hillary

########################################## Load moud data
bup <- load_data("moud_bup_intervals.fst", drv_root)
methadone <- load_data("moud_methadone_intervals.fst", drv_root)
naltrexone <- load_data("moud_nal_intervals.fst", drv_root)

moud <- rbind(bup, methadone, naltrexone)

setDT(moud)
setkey(moud, BENE_ID)

moud <- moud[, .(BENE_ID, moud_start_dt, moud_end_dt)] |>
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |>
  distinct()
##########################################


# Read in IPH dataset
iph <- open_iph()

# Read in OTH 
oth <- open_oth()

iph_hillary <-
  select(iph, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |>
  filter(!is.na(BENE_ID)) |>
  filter(SRVC_BGN_DT >= as.Date("2018-07-01")) |>
  collect() |> 
  # inner_join(cohort) |> 
  mutate(SRVC_BGN_DT = case_when(
    is.na(SRVC_BGN_DT) ~ SRVC_END_DT, 
    TRUE ~ SRVC_BGN_DT)
  ) |> 
  pivot_longer(cols = contains("DGNS_CD"), names_to = "dg_num", values_to = "cd") |> 
  drop_na(cd) |> 
  mutate(oud_hillary_dt = case_when(cd %in% codes ~ SRVC_BGN_DT)) |>  
  drop_na(oud_hillary_dt) |> 
  distinct(BENE_ID, oud_hillary_dt)

oth_hillary <-
  select(oth, BENE_ID, SRVC_BGN_DT, SRVC_END_DT, contains("DGNS_CD")) |> 
  filter(!is.na(BENE_ID)) |>
  filter(SRVC_BGN_DT >= as.Date("2018-07-01")) |>
  # inner_join(cohort) |> 
  collect() |> 
  filter(!(is.na(DGNS_CD_1) & is.na(DGNS_CD_2))) |> 
  mutate(SRVC_BGN_DT = case_when(is.na(SRVC_BGN_DT) ~ SRVC_END_DT, TRUE ~ SRVC_BGN_DT)) |> 
  # define oud variable of interest via codes
  mutate(oud_hillary_dt = case_when(DGNS_CD_1 %in% codes ~ SRVC_BGN_DT,
                                    DGNS_CD_2 %in% codes ~ SRVC_BGN_DT)) |>
  drop_na(oud_hillary_dt) |> # drop anyone who doesn't have a date for the oud var of interest
  distinct(BENE_ID, oud_hillary_dt)

oud_hillary <- 
  bind_rows(iph_hillary, oth_hillary) |> 
  distinct()

# oud_hillary <- 
#   inner_join(oud_hillary, cohort) |> 
#   filter(oud_hillary_dt %within% interval(washout_start_dt, exposure_end_dt + 455))

# write_data(oud_hillary, "all_oud_hillary.fst", drv_root)



# 3 cohorts -----------------------------------------------------------------
# 1st: ends on December 17th
# 2nd: ends on November 17th
# 3rd: ends on July 4th
# oud_hillary <- load_data("all_oud_hillary.fst", drv_root)

# 1st --------------------
oud_hillary_dec17 <- oud_hillary |>
  filter(oud_hillary_dt >= as.Date("2019-01-01"),
         oud_hillary_dt <= as.Date("2019-12-17")) |>
  group_by(BENE_ID) |>
  summarise(index_dt = min(oud_hillary_dt))


cohort_dec17 <- oud_hillary_dec17 |>
  left_join(oud_hillary) |>
  filter(oud_hillary_dt < index_dt,
         oud_hillary_dt >= index_dt %m-% days(182)) |>
  mutate(exclusion_dec17_washout = 1) |>
  select(BENE_ID, index_dt, exclusion_dec17_washout) |>
  distinct() |>
  right_join(oud_hillary_dec17)


cohort_oud_hillary <- cohort_dec17 |>
  filter(is.na(exclusion_dec17_washout)) |> # keep only people who do not have a washout OUD 
  mutate(washout_start_dt = index_dt %m-% days(182),
         exclusion_dec17_washout = 0, 
         exclusion_nov17_washout = ifelse(index_dt > as.Date("2019-11-17"), 1, 0), # eligible for the 44 day follow-up period?
         exclusion_jul4_washout = ifelse(index_dt > as.Date("2019-07-04"), 1, 0)) # eligible for the 180 day follow-up period?


############### Adding additional restriction to the jul4 cohort, where an MOUD claim must exist between OUD diagnosis and Jul 4

# filter to MOUD claims that come after OUD diagnosis
cohort_jul4_moud_requirement <- cohort_oud_hillary |>
  left_join(moud) |>
  filter(moud_start_dt >= index_dt) |>
  as.data.table()

# filter to those who initiated MOUD on or before July 4th
cohort_jul4_moud_requirement <- cohort_jul4_moud_requirement[, .SD[min(moud_start_dt) <= as.Date("2019-07-04")], by = BENE_ID] |>
  pull(BENE_ID) |>
  unique()

# modify inclusion flag for July 4th cohort
cohort_oud_hillary <- cohort_oud_hillary |>
  mutate(exclusion_jul4_washout = ifelse(BENE_ID %in% cohort_jul4_moud_requirement, 0, 1))

write_data(cohort_oud_hillary, "cohort_oud_hillary.fst", drv_root)




