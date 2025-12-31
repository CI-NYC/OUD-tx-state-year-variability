# -------------------------------------
# Script: 01_01_filter_continuous_enrollment.R
# Author: Nick Williams
# Purpose: Split enrollment periods into chunks per beneficiary
# Notes: This is checking whether patients are enrolled from their washout_start_dt
#         until 180 days after their MOUD initiation (rather than OUD diagnosis, 
#         unlike the dec17 and nov17)
#         Also note, in this cohort, the latest allowed index date is July 4th 
# -------------------------------------

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(foreach)
library(doFuture)
library(dplyr)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

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


# Load washout dates
washout <- load_data("cohort_oud_hillary.fst", drv_root) |> as.data.table() |>
  filter(exclusion_jul4_washout == 0) |>
  select(BENE_ID, washout_start_dt, index_dt)

# filter to MOUD claims that come after OUD diagnosis
washout <- washout |>
  left_join(moud) |>
  filter(moud_start_dt >= index_dt)

# setting "exposure_end_dt". This date, 6 month after moud initation, will be used to check censoring
washout <- washout[, .(
  washout_start_dt = first(washout_start_dt),
  index_dt = first(index_dt),
  exposure_end_dt = min(moud_start_dt) + days(180)
), by = BENE_ID]


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
                      "/tmp_jul4/enrollment_period_chunk_", 
                      i, ".rds"
  )
  saveRDS(tmp[chunks[[i]]], file = file_name)
}
