# -------------------------------------
# Script: 01_02_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Create continuous enrollment periods and filter 
#   to those periods within the study timeframe.
# Notes:
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

# Load washout dates
washout <- load_data("cohort_oud_hillary.fst", drv_root) |> as.data.table() |>
  filter(exclusion_jul4_washout == 0) |>
  select(BENE_ID, washout_start_dt, index_dt)

# Load temporary files for 01_01_filter_continuous_enrollment.R
files <- 
  file.path(drv_root, "tmp_jul4") |> 
  list.files(full.names = TRUE)

#' Creates continuous enrollment periods
find_enrollment_periods <- function(data) {
  # If there is only one row, return that row directly
  if (nrow(data) == 1) {
    out <- data.table(BENE_ID = data$BENE_ID, 
                      washout_start_dt = data$washout_start_dt, 
                      index_dt = data$index_dt,
                      exposure_end_dt = data$exposure_end_dt, 
                      enrollment_start_dt = data$ENRLMT_START_DT, 
                      enrollment_end_dt = data$ENRLMT_END_DT)
  } else {
    # Create intervals for each enrollment period
    enrollment_periods <- interval(data$ENRLMT_START_DT, data$ENRLMT_END_DT)
    
    # Create vectors for start and end dates
    start_dates <- int_start(enrollment_periods)
    end_dates <- int_end(enrollment_periods)
    
    # Initialize the list of enrollment periods
    enrollment_period <- list()
    
    # Initialize current interval
    current_start <- start_dates[1]
    current_end <- end_dates[1]
    
    # Loop through each interval
    for (i in 2:length(enrollment_periods)) {
      if (end_dates[i - 1] + days(1) == start_dates[i]) {
        # If current end + 1 day is the start of next interval, merge them
        current_end <- end_dates[i]
      } else {
        # Add current interval to the list and reset current interval
        enrollment_period <- append(enrollment_period, interval(current_start, current_end))
        current_start <- start_dates[i]
        current_end <- end_dates[i]
      }
    }
    
    # Add the last interval
    enrollment_period <- append(enrollment_period, interval(current_start, current_end))
    
    # Return result as a data.table
    out <- data.table(BENE_ID = data$BENE_ID[1], 
                      washout_start_dt = data$washout_start_dt[1], 
                      index_dt = data$index_dt[1],
                      exposure_end_dt = data$exposure_end_dt[1], 
                      enrollment_start_dt = as.Date(int_start(enrollment_period)), 
                      enrollment_end_dt = as.Date(int_end(enrollment_period)))
  }
  
  out[(washout_start_dt >= enrollment_start_dt) & (exposure_end_dt <= enrollment_end_dt)]
}


plan(multisession, workers = 20)

for (i in seq_along(files)) {
  tmp <- readRDS(files[i])
  
  valid_periods <- 
    foreach(x = tmp, .combine = "rbind", .options.future = list(chunk.size = 5e3)) %dofuture% {
      find_enrollment_periods(x)
    }
  
  write_data(
    valid_periods, 
    paste0("enrollment_period_chunk_", i, ".fst"), 
    file.path(drv_root, "valid_enrollment_periods_jul4")
  )
}

rm(valid_periods)
gc()

plan(sequential)

cohort <- 
  file.path(drv_root, "valid_enrollment_periods_jul4") |> 
  list.files(full.names = TRUE) |> 
  lapply(\(x) read_fst(x, columns = "BENE_ID", as.data.table = TRUE)) |> 
  rbindlist()

washout <- merge(washout, cohort)

# export
write_data(distinct(washout), "hillary_jul4_washout_continuous_enrollment_dts.fst", drv_root)
