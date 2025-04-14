# -------------------------------------
# Script: 08_baseline_covariates.R
# Author: Nick Williams
# Purpose: Add baseline covariates to data.
# Notes:
# -------------------------------------

library(tidyverse)
library(fst)
library(collapse)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")
# drv_root <- "/mnt/general-data/disability/pain-severity/undertreated-pain-cohort"

# load demographics dataset
demo <- open_demo()

# load cohort
cohort <- load_data("hillary_cohort_with_exclusions_noncontinuous_enrollment.fst", file.path(drv_root, "modelling"))

initiation_metric <- load_data("initiation_metric.fst", drv_root) |> 
  select(BENE_ID, initation_metric = moud_initiation) |>
  mutate(C_14 = 1)

engagement_metric <- load_data("engagement_metric.fst", drv_root) |> 
  select(BENE_ID, ends_with("metric")) |>
  mutate(C_44 = 1)

retention_metric <- load_data("retention_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric")) |>
  mutate(C_180 = 1)

counseling_metric <- load_data("counseling_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))

cohort <- cohort |>
  left_join(initiation_metric) |>
  left_join(engagement_metric) |>
  left_join(retention_metric) |>
  left_join(counseling_metric) |>
  mutate(across(c("C_14","C_44","C_180"), ~ replace_na(., 0)))

demo <- 
  filter(demo, BENE_ID %in% cohort$BENE_ID) |> 
  collect() |> 
  select(BENE_ID,
         RFRNC_YR,
         BIRTH_DT,
         BENE_STATE_CD,
         BENE_CNTY_CD,
         SEX_CD,
         ETHNCTY_CD,
         RACE_ETHNCTY_CD,
         RACE_ETHNCTY_EXP_CD,
         PRMRY_LANG_GRP_CD,
         PRMRY_LANG_CD,
         INCM_CD,
         VET_IND,
         HSEHLD_SIZE_CD,
         MRTL_STUS_CD,
         TANF_CASH_CD,
         SSI_STATE_SPLMT_CD,
         TPL_INSRNC_CVRG_IND) |> 
  distinct()

fill_vals <- function(data, x) {
  select(data, BENE_ID, RFRNC_YR, one_of(x))  |>
    roworder(BENE_ID, RFRNC_YR) |>
    filter(if (x == "BIRTH_DT" | x == "SEX_CD") !is.na(.data[[x]]) else TRUE) |> # these values shouldn't change
    group_by(BENE_ID) |>
    filter(row_number() == 1) |>
    fselect(-RFRNC_YR) |>
    ungroup() 
}

set.seed(1)
covar <- map(names(demo)[3:fncol(demo)], \(x) fill_vals(demo, x))
covar <- reduce(covar, left_join)

cohort <- 
  left_join(cohort, covar) |> 
  # left_join(probable_income) |> 
  mutate(
    dem_age = floor(time_length(interval(BIRTH_DT, index_dt), "years")),
    dem_race = case_when(
      RACE_ETHNCTY_CD == "1" ~ "White, non-Hispanic",
      RACE_ETHNCTY_CD == "2" ~ "Black, non-Hispanic",
      RACE_ETHNCTY_CD == "3" ~ "Asian, non-Hispanic",
      RACE_ETHNCTY_CD == "4" ~ "American Indian and Alaska Native (AIAN), non-Hispanic",
      RACE_ETHNCTY_CD == "5" ~ "Hawaiian/Pacific Islander",
      RACE_ETHNCTY_CD == "6" ~ "Multiracial, non-Hispanic",
      RACE_ETHNCTY_CD == "7" ~ "Hispanic, all races"
    ),
    dem_primary_language_english = case_when(
      PRMRY_LANG_GRP_CD == "E" ~ 1,
      PRMRY_LANG_GRP_CD != "E" ~ 0
    ),
    dem_married_or_partnered = case_when(
      as.numeric(MRTL_STUS_CD) <= 8 ~ 1,
      as.numeric(MRTL_STUS_CD) >= 8 ~ 0
    ),
    dem_household_size = case_when(
      HSEHLD_SIZE_CD == "01" ~ "1",
      HSEHLD_SIZE_CD == "02" ~ "2",
      as.numeric(HSEHLD_SIZE_CD) > 2 ~ "2+"
    ),
    dem_veteran = as.numeric(VET_IND),
    dem_tanf_benefits = case_when(
      TANF_CASH_CD == "2" ~ 1,
      TANF_CASH_CD == "1" ~ 0,
      TANF_CASH_CD == "0" ~ 0
    ),
    dem_ssi_benefits = case_when(
      SSI_STATE_SPLMT_CD == "000" ~ "Not Applicable",
      SSI_STATE_SPLMT_CD %in% c("001","002") ~ "Mandatory or optional"
    ),
    dem_sex = SEX_CD,
    # dem_probable_high_income = case_when(probable_high_income_cal == 1 ~ 1,
    #                                      as.numeric(INCM_CD) >= 3 ~ 1, # https://resdac.org/cms-data/variables/income-relative-federal-poverty-level-latest-year
    #                                      TRUE ~ 0)
  ) |> 
  # left_join(urbanicity) |>
  # left_join(adhd |> select(BENE_ID, adhd_washout_cal)) |>
  # left_join(anxiety |> select(BENE_ID, anxiety_washout_cal)) |>
  # left_join(bipolar |> select(BENE_ID, bipolar_washout_cal)) |>
  # left_join(depression |> select(BENE_ID, depression_washout_cal)) |>
  # left_join(mental_ill |> select(BENE_ID, mental_ill_washout_cal)) |>
  select(BENE_ID, 
         ends_with("dt", ignore.case = FALSE),
         ends_with("washout"),
         starts_with("C"),
         ends_with("metric"),
         starts_with("dem"),
         # starts_with("exposure"), 
         # starts_with("subset"), 
        # cens_period_1, oud_period_1,
        # cens_period_2, oud_period_2,
        # cens_period_3, oud_period_3,
        # cens_period_4, oud_period_4,
        # cens_period_5, oud_period_5,
        # cens_hillary_period_1, oud_hillary_period_1,
        # cens_hillary_period_2, oud_hillary_period_2,
        # cens_hillary_period_3, oud_hillary_period_3,
        # cens_hillary_period_4, oud_hillary_period_4,
        # cens_hillary_period_5, oud_hillary_period_5
        )

write_data(cohort, "hillary_cohort_baseline_characteristics.fst", file.path(drv_root, "modelling"))

