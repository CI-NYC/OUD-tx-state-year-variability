# -------------------------------------
# Script: merge_metrics
# Author: Anton Hung
# Purpose: Generate supplementary tables showing numerator counts, denominator counts, and metric counts.
# Notes:
# -------------------------------------

library(arrow)
library(data.table)
library(yaml)
library(arrow)
library(tidyverse)
library(collapse)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)
urbanicity <- load_data("urbanicity.fst", drv_root)

initiation_metric <- load_data("initiation_metric.fst", drv_root) |> select(BENE_ID, initation_metric = moud_initiation)
engagement_metric <- load_data("engagement_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))
retention_metric <- load_data("retention_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))
counseling_metric <- load_data("counseling_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))

# State --------------------------
demo <- open_demo()

demo <- right_join(demo, cohort) |> 
  collect()

state <-
  select(demo, BENE_ID, index_dt, RFRNC_YR, STATE_CD) |>
  filter(year(index_dt) == as.numeric(RFRNC_YR)) |>
  group_by(BENE_ID) |>
  arrange(STATE_CD) |>
  summarise(STATE_CD = first(STATE_CD))

cohort <- cohort |> 
  join(urbanicity, how = "left") |>
  join(state, how = "left") |>
  join(initiation_metric, how = "left") |>
  join(engagement_metric, how = "left") |>
  join(retention_metric, how = "left") |>
  join(counseling_metric, how = "left") |>
  mutate(urbanicity = factor(urbanicity, levels = c("Urban", "Suburban", "Rural", "Missing")))

# State
initiation_by_state <- cohort |>
  arrange(STATE_CD) |>
  group_by(STATE_CD) |>
  summarise(initiation_n = sum(initation_metric, na.rm=T),
            strata_n = n(),
            initiation_prop = round(mean(initation_metric, na.rm=T)*100,1),
            initiation_prop_check = round(mean(initiation_n/strata_n*100, na.rm=T),1),
            check_succeed = initiation_prop == initiation_prop_check)

engagement_by_state <- cohort |>
  arrange(STATE_CD) |>
  group_by(STATE_CD) |>
  summarise(engagement_n = sum(engagement_metric, na.rm=T),
            strata_n = sum(exclusion_nov17 == 0),
            engagement_prop = round(mean(engagement_metric*100, na.rm=T),1),
            engagement_prop_check = round(mean(engagement_n/strata_n*100, na.rm=T),1),
            check_succeed = engagement_prop == engagement_prop_check)

retention_by_state <- cohort |>
  arrange(STATE_CD) |>
  group_by(STATE_CD) |>
  summarise(retention_n = sum(retention_metric, na.rm=T),
            retention_n = ifelse(retention_n >= 11, retention_n, NA),
            strata_n = sum(exclusion_jul4 == 0),
            retention_prop = round(mean(retention_metric*100, na.rm=T),1),
            retention_prop_check = round(mean(retention_n/strata_n*100, na.rm=T),1),
            check_succeed = retention_prop == retention_prop_check)

counseling_by_state <- cohort |>
  arrange(STATE_CD) |>
  group_by(STATE_CD) |>
  summarise(counseling_n = sum(counseling_metric, na.rm=T),
            counseling_n = ifelse(counseling_n >= 11, counseling_n, NA),
            strata_n = sum(exclusion_nov17 == 0),
            counseling_prop = round(mean(counseling_metric*100, na.rm=T),1),
            counseling_prop_check = round(mean(counseling_n/strata_n*100, na.rm=T),1),
            check_succeed = counseling_prop == counseling_prop_check)

table_one_by_state <- cbind(initiation_by_state |> select(State = STATE_CD, 
                                                          `Initiated MOUD (N)` = initiation_n,
                                                          `Has OUD and continuously enrolled for 14 days (N)` = strata_n,
                                                          `Initation metric (%)` = initiation_prop),
                            engagement_by_state |> select(`Engaged in care (N)` = engagement_n,
                                                          `Has OUD and continuously enrolled for 44 days (N)` = strata_n,
                                                          `Engagement metric (%)` = engagement_prop),
                            retention_by_state |> select(`180 days continuously on MOUD (N)` = retention_n,
                                                          `Has OUD and continuously enrolled for 180 days (N)` = strata_n,
                                                          `Retention metric (%)` = retention_prop_check)
                            # counseling_by_state |> select(`Received mental health counseling (N)` = counseling_n,
                            #                               `Has OUD and continuously enrolled for 44 days (N)` = strata_n,
                            #                               `Counseling metric (%)` = counseling_prop)
                            )

# Urbanicity
initiation_by_urbanicity <- cohort |>
  group_by(urbanicity) |>
  summarise(initiation_n = sum(initation_metric, na.rm=T),
            strata_n = n(),
            initiation_prop = round(mean(initation_metric*100, na.rm=T),1),
            initiation_prop_check = round(mean(initiation_n/strata_n*100, na.rm=T),1),
            check_succeed = initiation_prop == initiation_prop_check)

engagement_by_urbanicity <- cohort |>
  group_by(urbanicity) |>
  summarise(engagement_n = sum(engagement_metric, na.rm=T),
            strata_n = sum(exclusion_nov17 == 0),
            engagement_prop = round(mean(engagement_metric*100, na.rm=T),1),
            engagement_prop_check = round(mean(engagement_n/strata_n*100, na.rm=T),1),
            check_succeed = engagement_prop == engagement_prop_check)

retention_by_urbanicity <- cohort |>
  group_by(urbanicity) |>
  summarise(retention_n = sum(retention_metric, na.rm=T),
            retention_n = ifelse(retention_n >= 11, retention_n, NA),
            strata_n = sum(exclusion_jul4 == 0),
            retention_prop = round(mean(retention_metric*100, na.rm=T),1),
            retention_prop_check = round(mean(retention_n/strata_n*100, na.rm=T),1),
            check_succeed = retention_prop == retention_prop_check)

counseling_by_urbanicity <- cohort |>
  group_by(urbanicity) |>
  summarise(counseling_n = sum(counseling_metric, na.rm=T),
            counseling_n = ifelse(counseling_n >= 11, counseling_n, NA),
            strata_n = sum(exclusion_nov17 == 0),
            counseling_prop = round(mean(counseling_metric*100, na.rm=T),1),
            counseling_prop_check = round(mean(counseling_n/strata_n*100, na.rm=T),1),
            check_succeed = counseling_prop == counseling_prop_check)

table_two_by_urbanicity <- cbind(initiation_by_urbanicity |> select(Urbanicity = urbanicity, 
                                                          `Initiated MOUD (N)` = initiation_n,
                                                          `Has OUD and continuously enrolled for 14 days (N)` = strata_n,
                                                          `Initation metric (%)` = initiation_prop),
                            engagement_by_urbanicity |> select(`Engaged in care (N)` = engagement_n,
                                                          `Has OUD and continuously enrolled for 44 days (N)` = strata_n,
                                                          `Engagement metric (%)` = engagement_prop),
                            retention_by_urbanicity |> select(`180 days continuously on MOUD (N)` = retention_n,
                                                         `Has OUD and continuously enrolled for 180 days (N)` = strata_n,
                                                         `Retention metric (%)` = retention_prop_check)
                            # counseling_by_urbanicity |> select(`Received mental health counseling (N)` = counseling_n,
                            #                               `Has OUD and continuously enrolled for 44 days (N)` = strata_n,
                            #                               `Counseling metric (%)` = counseling_prop)
                            )

write.csv(table_one_by_state, "~/medicaid/OUD_tx_state_year_variability/data/private/table_one_by_state.csv")
write.csv(table_two_by_urbanicity, "~/medicaid/OUD_tx_state_year_variability/data/private/table_two_by_urbanicity.csv")
