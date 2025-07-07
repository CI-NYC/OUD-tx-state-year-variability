# -------------------------------------
# Script: 01_01_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Split enrollment periods into chunks per beneficiary
# Notes: This is checking whether patients are enrolled from their washout_start_dt
#         until 14 days after their first OUD diagnosis.
#         Also note, in this cohort, the latest allowed index date is Dec 17th 
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
  select(BENE_ID, washout_start_dt, index_dt)

washout[, let(exposure_end_dt = index_dt + days(14))]

# Load all dates
dates <- open_dedts()

dates <- 
  filter(dates, !is.na(BENE_ID)) |> 
  select(BENE_ID, ENRLMT_START_DT, ENRLMT_END_DT) |>
  inner_join(washout, by = "BENE_ID") |> 
  collect()

setDT(dates, key = "BENE_ID")

dates <- dates[order(rleid(BENE_ID), ENRLMT_START_DT)]
dates <- dates[!is.na(ENRLMT_START_DT) & !is.na(ENRLMT_END_DT)]
dates <- dates[ENRLMT_START_DT <= exposure_end_dt]

idx <- split(seq_len(nrow(dates)), list(dates$BENE_ID))
tmp <- lapply(idx, \(x) dates[x])

rm(idx, washout, dates)
gc()

# Define the function to split a list into chunks
split_list_into_chunks <- function(lst, chunk_size) {
  split(seq_along(lst), ceiling(seq_along(lst) / chunk_size))
}

chunks <- split_list_into_chunks(tmp, 1e5)

# Save each chunk to a separate RDS file
for (i in seq_along(chunks)) {
  file_name <- paste0(drv_root,
    "/tmp_dec17/enrollment_period_chunk_", 
    i, ".rds"
  )
  saveRDS(tmp[chunks[[i]]], file = file_name)
}
