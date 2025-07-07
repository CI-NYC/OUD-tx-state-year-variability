
library(dplyr)
library(data.table)
library(arrow)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

urbanicity <- open_dataset("/mnt/general-data/disability/disenrollment/tafdebse/dem_df.parquet") |>
  select(BENE_ID, RUCC_2013) |>
  filter(!is.na(RUCC_2013)) |>
  collect()

cohort <- cohort |>
  left_join(urbanicity) |>
  mutate(urbanicity = case_when(
    RUCC_2013 %in% 1:3 ~ "Urban",
    RUCC_2013 %in% 4:6 ~ "Suburban",
    RUCC_2013 %in% 7:9 ~ "Rural",
    TRUE             ~ "Missing"
  )) |>
  select(BENE_ID, urbanicity)

write_data(cohort, "urbanicity.fst", drv_root)
