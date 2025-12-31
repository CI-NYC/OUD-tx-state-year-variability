# -------------------------------------
# Script: 
# Author: Anton Hung (adapted from Shodai Inose)
# Purpose: Compute retention metric variable
#          Numerator: those who remain moud continuously for 180 days after 
#                     initiation, allowing for 7 day gaps.
#          Denominator: those who initated moud within 14 days and remain 
#                       continuously enrolled for 180 days after moud.
# Notes: Modified from https://github.com/CI-NYC/everything-local-lmtp/blob/main/scripts/00_create_cohort/05_exposure/03_days_supply.R
# -------------------------------------

library(tidyverse)
library(readxl)
library(fst)
library(lubridate)
library(data.table)
library(foreach)
library(doFuture)
library(collapse)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

# load cohort and opioid data
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root) |>
  filter(exclusion_jul4 == 0) #|>
  # mutate(follow_up_end_dt = index_dt %m+% days(180))

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
  filter(moud_start_dt >= index_dt)

moud <- moud[, .SD[min(moud_start_dt) <= as.Date("2019-07-04")], by = BENE_ID]

moud_dates <- moud[, .(
  moud_start_dt = min(moud_start_dt), 
  follow_up_until_dt = min(moud_start_dt) + days(180)
), by = BENE_ID]

# Calculate duration -----------------------------------------------------
moud <- moud[, list(data = list(data.table(.SD))), by = BENE_ID]

get_duration <- function(data, gap = 7) {
  #data <- (opioids |> filter(BENE_ID == "TEST"))$data |> as.data.table() #for testing
  # gap <- 30
  to_modify <- copy(data) |>
    as.data.table()
  
  data <- data |>
    as.data.table()
  
  # all dates with some opioid prescription
  opioid_dates <- to_modify[, .(date = seq(moud_start_dt, moud_end_dt, by = "1 day")), 
                            by = .(seq_len(nrow(to_modify)))
  ][, seq_len := 1] |> distinct()
  
  # all dates in hypothetical exposure period
  all_dates_exposure_period <- data[, .(date = seq(moud_start_dt, moud_start_dt %m+% days(180), by = "1 day")), 
                                    by = .(seq_len(nrow(data)))
  ][seq_len == 1][, seq_len := NULL]
  
  # join opioid dates with all possible dates in exposure period
  all_dates_exposure_period <- merge(all_dates_exposure_period, opioid_dates, by = "date", all.x = TRUE)[, seq_len := ifelse(is.na(seq_len), 0, seq_len)][date < as.Date("2020-01-01"),] # exposure cannot go past 12-31-2019
  
  # get cumulative day sum
  all_dates_exposure_period[, opioid_days := cumsum(seq_len)]
  
  # group by instance to identify gaps (anything > 0 indicates a gap of X days)
  all_dates_exposure_period[, num_days_in_gap := .N - 1, by = opioid_days]
  
  # find FIRST instance of 30+ day gap -- this is the last day of exposure
  all_dates_exposure_period[, indicator_7_plus_day_gap := as.integer(.I == min(.I[num_days_in_gap > gap])), by = opioid_days]
  
  # if all instances of indicator_30_plus_day_gap are 0, then the last row is returned
  if (all(all_dates_exposure_period[, indicator_7_plus_day_gap] == 0)) {
    # find the final exposure date
    final_exposure <- all_dates_exposure_period[seq_len == 1, max(date)]
    all_dates_exposure_period <- all_dates_exposure_period[date <= final_exposure]
    all_dates_exposure_period[.N, indicator_7_plus_day_gap := 1]
  }
  
  # changing column names
  setnames(all_dates_exposure_period, old = c("date", "opioid_days"), new = c("exposure_end_dt", "exposure_days_supply"))
  
  # return last exposure date + number of days supplied
  all_dates_exposure_period <- all_dates_exposure_period[indicator_7_plus_day_gap == 1]
  
  # get only first instance of 30 day gap (if multiple)
  all_dates_exposure_period <- all_dates_exposure_period[exposure_end_dt == min(exposure_end_dt)]
  
  # keeping only exposure end date and days' supply
  all_dates_exposure_period <- all_dates_exposure_period[, .(exposure_end_dt, exposure_days_supply)]
  
  all_dates_exposure_period
}


plan(multisession, workers = 10)

# Apply function
out <- foreach(data = moud$data,
               id = moud$BENE_ID,
               .combine = "rbind",
               .options.future = list(chunk.size = 1e4)) %dofuture% {
                 out <- get_duration(data, gap = 7)
                 out$BENE_ID <- id
                 setcolorder(out, "BENE_ID")
                 out
               }

plan(sequential)

result <- out |>
  left_join(moud_dates)|>
  mutate(retention_metric = as.numeric(follow_up_until_dt <= exposure_end_dt))
# 
# cohort <- cohort |>
#   left_join(out) |>
#   mutate(retention_metric = ifelse(!is.na(follow_up_end_dt) & 
#                                    follow_up_end_dt == exposure_end_dt, 1, 0),
#          retention_metric = ifelse(is.na(follow_up_end_dt), 0, retention_metric))

write_data(result, "retention_metric.fst", drv_root)

