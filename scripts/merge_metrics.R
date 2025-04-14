library(arrow)
library(data.table)
library(yaml)
library(arrow)
library(tidyverse)
library(collapse)

set.seed(1)

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
  fsubset(year(index_dt) == as.numeric(RFRNC_YR)) |>
  group_by(BENE_ID) |>
  summarise(STATE_CD = first(STATE_CD))

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
  summarise(initiation_prop = mean(initation_metric*100, na.rm=T),
            engagement_prop = mean(engagement_metric*100, na.rm=T),
            retention_prop = mean(retention_metric*100, na.rm=T),
            counseling_prop = mean(counseling_metric*100, na.rm=T))

median_calc <- function(metric){
  value <- round(median(metric),2)
  q25 <- round(quantile(metric, 0.25),2)
  q75 <- round(quantile(metric, 0.75),2)
  return(paste0(value, "% (", q25, ",", q75, ")"))
}

median_metrics <- c(median_calc(metrics_by_state$initiation_prop),
                    median_calc(metrics_by_state$engagement_prop),
                    median_calc(metrics_by_state$retention_prop),
                    median_calc(metrics_by_state$counseling_prop))

metric_names <- c("Initiation metric", "Engagement metric", "Retention metric", "Counseling metric")
data.frame(metric_names, overall_metrics, median_metrics)


# plots --------------------------
p <- ggplot(metrics_by_state, aes(x = initiation_prop, y = reorder(STATE_CD, desc(initiation_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  # xlab("Percentage of patients who initated MOUD") +
  xlab("Initiation metric (%)") +
  theme_light()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/initation_prop.png", p, height = 5, width = 8)

p <- ggplot(metrics_by_state, aes(x = engagement_prop, y = reorder(STATE_CD, desc(engagement_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  # xlab("Percentage of patients who engaged in care") +
  xlab("Engagement metric (%)") +
  theme_light()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/engagement_prop.png", p, height = 5, width = 8)

p <- ggplot(metrics_by_state, aes(x = retention_prop, y = reorder(STATE_CD, desc(retention_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  # xlab("Percentage of patients who received a continuous >180 days supply of MOUD") +
  xlab("Retention metric (%)") +
  theme_light()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/retention_prop.png", p, height = 5, width = 8)

p <- ggplot(metrics_by_state, aes(x = counseling_prop, y = reorder(STATE_CD, desc(counseling_prop)))) +
  geom_bar(stat="identity") +
  ylab("State") +
  # xlab("Percentage of patients who received mental health counseling") +
  xlab("Counseling metric (%)") +
  theme_light()
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/counseling_prop.png", p, height = 5, width = 8)


boxplot_df <- rbind(metrics_by_state |> select(STATE_CD, value = initiation_prop) |> mutate(treatment = "initiation"),
                    metrics_by_state |> select(STATE_CD, value = engagement_prop) |> mutate(treatment = "engagement"),
                    metrics_by_state |> select(STATE_CD, value = retention_prop) |> mutate(treatment = "retention"),
                    metrics_by_state |> select(STATE_CD, value = counseling_prop) |> mutate(treatment = "counseling"))
boxplot_df$treatment <- factor(boxplot_df$treatment, levels = c("counseling", "retention", "engagement", "initiation"))

p <- ggplot(boxplot_df, aes(x = value, y = treatment)) +
  geom_boxplot(fill="grey", width=0.5) +
  ylab("Treatment Quality Metric") +
  xlab("Percentage") +
  scale_y_discrete(labels = c("Initiation metric",
                              "Engagement metric",
                              "Retention metric",
                              "Counseling metric")) +
  theme_light()
p
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/metrics_distribution_by_state.png",p, width=8,height=4)
