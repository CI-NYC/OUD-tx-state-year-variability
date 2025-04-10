
source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")

cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

bup <- load_data("moud_bup_intervals.fst", drv_root)
methadone <- load_data("moud_methadone_intervals.fst", drv_root)
naltrexone <- load_data("moud_nal_intervals.fst", drv_root)

moud <- rbind(bup, methadone, naltrexone)

moud <- 
  roworder(moud, BENE_ID, moud_start_dt) |> 
  join(cohort, how = "left") |> 
  fmutate(moud_initiation = moud_start_dt %within% interval(index_dt, index_dt %m+% days(14))
  #           int_overlaps(
  #   interval(moud_start_dt, moud_end_dt),
  #   interval(index_dt, index_dt %m+% days(14))
  # )
  ) |> 
  fgroup_by(BENE_ID) |> 
  fsummarise(moud_initiation = as.numeric(sum(moud_initiation) > 0))

# - Rejoin entire initial cohort and save
moud <- 
  join(cohort, moud, how = "left") |> 
  fmutate(moud_initiation = replace_na(moud_initiation, 0))

write_data(moud, "initiation_metric.fst", drv_root)
