library(tidyverse)
library(data.table)
library(collapse)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

bup <- load_data("moud_bup_intervals.fst", drv_root)
methadone <- load_data("moud_methadone_intervals.fst", drv_root)
naltrexone <- load_data("moud_nal_intervals.fst", drv_root)

moud <- rbind(bup, methadone, naltrexone)

setDT(moud)
setkey(moud, BENE_ID)

moud <- moud[, .(BENE_ID, moud_start_dt, moud_end_dt)] |>
  arrange(BENE_ID, moud_start_dt, moud_end_dt) |>
  distinct()

time_to_initiation <- cohort |> 
  left_join(moud) |>
  filter(moud_start_dt %within% interval(index_dt, index_dt %m+% days(14))) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(index_dt = min(index_dt),
             initation_dt = min(moud_start_dt)) |>
  fmutate(time_to_initiation = time_length(interval(index_dt, initation_dt),"days"))


# # - Rejoin entire initial cohort and save
# moud <- 
#   join(cohort, moud, how = "left") |> 
#   fmutate(moud_initiation = replace_na(moud_initiation, 0))

write_data(time_to_initiation, "time_to_initiation.fst", drv_root)

ggplot(time_to_initiation, aes(x=time_to_initiation)) +
  geom_bar()
