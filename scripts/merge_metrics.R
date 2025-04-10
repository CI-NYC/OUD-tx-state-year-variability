library(arrow)
library(data.table)
library(yaml)
library(arrow)
library(tidyverse)
library(collapse)


source("~/medicaid/OUD_tx_state_year_variability//R/helpers.R")
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root)

initiation_metric <- load_data("initiation_metric.fst", drv_root) |> select(BENE_ID, initation_metric = moud_initiation)
engagement_metric <- load_data("engagement_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))
retention_metric <- load_data("retention_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))
counseling_metric <- load_data("counseling_metric.fst", drv_root) |> select(BENE_ID, ends_with("metric"))

# State --------------------------
demo <- open_demo()

demo <- right_join(demo, cohort) |> 
  collect()

state <-
  fselect(demo, BENE_ID, index_dt, RFRNC_YR, STATE_CD) |>
  fsubset(year(index_dt) == as.numeric(RFRNC_YR) &
          STATE_CD != "MD") |>
  fselect(BENE_ID, STATE_CD)

cohort <- cohort |> 
  join(state, how = "left") |>
  join(initiation_metric, how = "left") |>
  join(engagement_metric, how = "left") |>
  join(retention_metric, how = "left") |>
  join(counseling_metric, how = "left")
  

# overall metrics ----------------
overall_metrics <- paste0(c(round(mean(cohort$initation_metric)*100,2),
                             round(mean(cohort$engagement_metric, na.rm=T)*100,2),
                             round(mean(cohort$retention_metric, na.rm=T)*100,2),
                             round(mean(cohort$counseling_metric, na.rm=T)*100,2)),"%")


metrics_by_state <- cohort |>
  group_by(STATE_CD) |>
  summarise(initiation_prop = mean(initation_metric, na.rm=T),
            engagement_prop = mean(engagement_metric, na.rm=T),
            retention_prop = mean(retention_metric, na.rm=T),
            counseling_prop = mean(counseling_metric, na.rm=T))

median_calc <- function(metric){
  value <- round(median(metric)*100,2)
  q25 <- round(quantile(metric, 0.25)*100,2)
  q75 <- round(quantile(metric, 0.75)*100,2)
  return(paste0(value, "% (", q25, ",", q75, ")"))
}

median_metrics <- c(median_calc(metrics_by_state$initiation_prop),
                    median_calc(metrics_by_state$engagement_prop),
                    median_calc(metrics_by_state$retention_prop),
                    median_calc(metrics_by_state$counseling_prop))


# plots --------------------------
p <- ggplot(metrics_by_state, aes(x = initiation_prop, y = reorder(STATE_CD, desc(initiation_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  xlab("Percentage of patients who initated MOUD") +
  theme_classic()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/initation_prop.pdf", p, height = 6, width = 8)

p <- ggplot(metrics_by_state, aes(x = engagement_prop, y = reorder(STATE_CD, desc(engagement_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  xlab("Percentage of patients who engaged in care") +
  theme_classic()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/engagement_prop.pdf", p, height = 6, width = 8)

p <- ggplot(metrics_by_state, aes(x = retention_prop, y = reorder(STATE_CD, desc(retention_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  xlab("Percentage of patients who received a continuous >180 days supply of MOUD") +
  theme_classic()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/retention_prop.pdf", p, height = 6, width = 8)

p <- ggplot(metrics_by_state, aes(x = counseling_prop, y = reorder(STATE_CD, desc(counseling_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  xlab("Percentage of patients who received mental health counseling") +
  theme_classic()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/counseling_prop.pdf", p, height = 6, width = 8)
