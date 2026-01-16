# -------------------------------------
# Script: merge_metrics
# Author: Anton Hung
# Purpose: Use the three metrics, initiation, engagement, retention, to generate final plots
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
  join(counseling_metric, how = "left")
  

# overall metrics ----------------
overall_metrics <- paste0(c(round(mean(cohort$initation_metric)*100,2),
                             round(mean(cohort$engagement_metric, na.rm=T)*100,2),
                             round(mean(cohort$retention_metric, na.rm=T)*100,2)
                             # round(mean(cohort$counseling_metric, na.rm=T)*100,2)
                            ),"%")

# State
# metrics_by_state <- cohort |>
#   group_by(STATE_CD) |>
#   summarise(initiation_prop = mean(initation_metric*100, na.rm=T),
#             engagement_prop = mean(engagement_metric*100, na.rm=T),
#             retention_prop = mean(retention_metric*100, na.rm=T),
#             counseling_prop = mean(counseling_metric*100, na.rm=T))

# Exploring counts to suppress
# metrics_by_state <- cohort |>
#   group_by(STATE_CD) |>
#   summarise(`Initiation metric` = sum(initation_metric, na.rm=T),
#             `Engagement metric` = sum(engagement_metric, na.rm=T),
#             `Retention metric` = sum(retention_metric, na.rm=T),
#             `Counseling metric` = sum(counseling_metric, na.rm=T)) |>
#   pivot_longer(cols = c(`Initiation metric`, `Engagement metric`, `Retention metric`, `Counseling metric`),
#                names_to = "metric", values_to = "Proportion (%)")

metrics_by_state <- cohort |>
  group_by(STATE_CD) |>
  summarise(`Initiation metric` = round(mean(initation_metric, na.rm=T)*100,1),
            `Engagement metric` = round(mean(engagement_metric, na.rm=T)*100,1),
            `Retention metric` = round(mean(retention_metric, na.rm=T)*100,1),
            `Counseling metric` = round(mean(counseling_metric, na.rm=T)*100,1)) |>
  pivot_longer(cols = c(`Initiation metric`, `Engagement metric`, `Retention metric`),
               names_to = "metric", values_to = "Proportion (%)") |>
  mutate(STATE_CD = fct_rev(fct_inorder(STATE_CD)),
         metric = fct_relevel(metric, "Initiation metric", "Engagement metric", "Retention metric")) |>
  mutate(`Proportion (%)` = ifelse(STATE_CD %in% c("HI","ND") & metric == "Retention metric", NA, `Proportion (%)`))

# Urbanicity
metrics_by_urbanicity <- cohort |>
  filter(!urbanicity=="Missing") |>
  group_by(urbanicity) |>
  summarise(`Initiation metric` = round(mean(initation_metric, na.rm=T)*100,1),
            `Engagement metric` = round(mean(engagement_metric, na.rm=T)*100,1),
            `Retention metric` = round(mean(retention_metric, na.rm=T)*100,1),
            `Counseling metric` = round(mean(counseling_metric, na.rm=T)*100,1)) |>
  pivot_longer(cols = c(`Initiation metric`, `Engagement metric`, `Retention metric`),
               names_to = "metric", values_to = "Proportion (%)") |>
  mutate(metric = fct_relevel(metric, "Initiation metric", "Engagement metric", "Retention metric"))



median_calc <- function(metric){
  value <- round(median(metric),2)
  q25 <- round(quantile(metric, 0.25),2)
  q75 <- round(quantile(metric, 0.75),2)
  return(paste0(value, "% (", q25, ",", q75, ")"))
}


metrics_by_group <- cohort |>
  group_by(STATE_CD) |>
  summarise(`Initiation metric` = round(mean(initation_metric, na.rm=T)*100,1),
            `Engagement metric` = round(mean(engagement_metric, na.rm=T)*100,1),
            `Retention metric` = round(mean(retention_metric, na.rm=T)*100,1),
            `Counseling metric` = round(mean(counseling_metric, na.rm=T)*100,1))

median_metrics <- c(median_calc(metrics_by_group$`Initiation metric`),
                    median_calc(metrics_by_group$`Engagement metric`),
                    median_calc(metrics_by_group$`Retention metric`)
                    # median_calc(metrics_by_state$counseling_prop)
                    )

metric_names <- c("Initiation metric", "Engagement metric", "Retention metric")
data.frame(metric_names, overall_metrics, median_metrics)


# plots --------------------------

threshold <- 6

p <- ggplot(metrics_by_state, aes(x = `Proportion (%)`, 
                                  y = fct_rev(fct_inorder(STATE_CD)))) +
  geom_bar(stat = "identity") +
  # Labels for bars too small to accommodate inside text:
  geom_text(
    data = subset(metrics_by_state, !is.na(`Proportion (%)`) & 
                    `Proportion (%)` < threshold),
    aes(label = `Proportion (%)`, 
        x = `Proportion (%)` * 1.1),
    hjust = -0.1,
    vjust = 0.5,
    size = 3.5,
    family="Arial",
    color = "black"
  ) +
  # Labels for bars big enough to hold the text inside:
  geom_text(
    data = subset(metrics_by_state, !is.na(`Proportion (%)`) & 
                    `Proportion (%)` >= threshold),
    aes(label = `Proportion (%)`, 
        x = `Proportion (%)` * 0.95),
    hjust = 1,
    vjust = 0.5,
    size = 3.5,
    family="Arial",
    color = "white"
  ) +
  # For rows where the metric is NA, show an asterisk:
  geom_text(
    data = subset(metrics_by_state, is.na(`Proportion (%)`)),
    aes(label = "*"),
    x = 0,
    hjust = -1,
    vjust = 0.7
  ) +
  ylab(NULL) +
  xlab("Percentage") +
  facet_wrap(~ metric, nrow = 1, ncol = 4) +
  theme_light() +
  theme(text = element_text(family = "Arial"),
        strip.text = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank())

p


ggsave("~/medicaid/OUD_tx_state_year_variability/data/public/metrics_by_state.tiff", p, height = 5, width = 7.5, dpi=300, compression="lzw")

# p <- ggplot(metrics_by_group, aes(x = initiation_prop, y = reorder(STATE_CD, desc(initiation_prop)))) +
#   geom_bar(stat="identity") +
#   ylab("State") +
#   # xlab("Percentage of patients who initated MOUD") +
#   xlab("Initiation metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/initation_prop.pdf", p, height = 5, width = 8)
# 
# p <- ggplot(metrics_by_group, aes(x = engagement_prop, y = reorder(STATE_CD, desc(engagement_prop)))) +
#   geom_bar(stat="identity") +
#   ylab("State") +
#   # xlab("Percentage of patients who engaged in care") +
#   xlab("Engagement metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/engagement_prop.pdf", p, height = 5, width = 8)
# 
# p <- ggplot(metrics_by_group, aes(x = retention_prop, y = reorder(STATE_CD, desc(retention_prop)))) +
#   geom_bar(stat="identity") +
#   ylab("State") +
#   # xlab("Percentage of patients who received a continuous >180 days supply of MOUD") +
#   xlab("Retention metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/retention_prop.pdf", p, height = 5, width = 8)
# 
# p <- ggplot(metrics_by_group, aes(x = counseling_prop, y = reorder(STATE_CD, desc(counseling_prop)))) +
#   geom_bar(stat="identity") +
#   ylab("State") +
#   # xlab("Percentage of patients who received mental health counseling") +
#   xlab("Counseling metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/counseling_prop.pdf", p, height = 5, width = 8)






# By Urbanicity
p <- ggplot(metrics_by_urbanicity, aes(x = `Proportion (%)`, y = urbanicity)) +
  geom_bar(stat="identity") +
  # Display the actual value when not NA
  geom_text(
    data = subset(metrics_by_urbanicity, !is.na(`Proportion (%)`)),
    aes(label = `Proportion (%)`, 
        x = `Proportion (%)` * 0.95),
    hjust = 1,
    vjust = 0.5,
    size = 4,
    color = "white"
  ) +
  ylab(NULL) +
  xlab("Percentage") +
  facet_wrap(~ metric, nrow = 1, ncol = 4) +
  theme_light() +
  theme(text = element_text(family = "Arial"),
        strip.text = element_text(face = "bold", color = "black"),
        axis.text.y = element_text(face = "bold"),
        panel.grid.major.y = element_blank())
ggsave("~/medicaid/OUD_tx_state_year_variability/data/public/metrics_by_urbanicity.tiff", p, height = 2.7, width = 7.5, dpi=300,compression="lzw")

# p <- ggplot(metrics_by_urbanicity, aes(x = engagement_prop, y = urbanicity)) +
#   geom_bar(stat="identity") +
#   ylab("Urbanicity") +
#   xlab("Engagement metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/engagement_prop_by_urbanicity.png", p, height = 5, width = 8)
# 
# p <- ggplot(metrics_by_urbanicity, aes(x = retention_prop, y = urbanicity)) +
#   geom_bar(stat="identity") +
#   ylab("Urbanicity") +
#   xlab("Retention metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/retention_prop_by_urbanicity.png", p, height = 5, width = 8)
# 
# p <- ggplot(metrics_by_urbanicity, aes(x = counseling_prop, y = urbanicity)) +
#   geom_bar(stat="identity") +
#   ylab("Urbanicity") +
#   xlab("Counseling metric (%)") +
#   theme_light()
# ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/counseling_prop_by_urbanicity.png", p, height = 5, width = 8)







boxplot_df <- rbind(metrics_by_group |> select(STATE_CD, value = initiation_prop) |> mutate(treatment = "initiation"),
                    metrics_by_group |> select(STATE_CD, value = engagement_prop) |> mutate(treatment = "engagement"),
                    metrics_by_group |> select(STATE_CD, value = retention_prop) |> mutate(treatment = "retention"),
                    metrics_by_group |> select(STATE_CD, value = counseling_prop) |> mutate(treatment = "counseling"))
boxplot_df$treatment <- factor(boxplot_df$treatment, levels = c("counseling", "retention", "engagement", "initiation"))

p <- ggplot(boxplot_df, aes(x = value, y = treatment)) +
  geom_boxplot(fill="grey", width=0.5) +
  ylab("Treatment Quality Metric") +
  xlab("Percentage") +
  scale_y_discrete(labels = c("Counseling metric",
                              "Retention metric",
                              "Engagement metric",
                              "Initiation metric")) +
  theme_light()
p
ggsave("~/medicaid/OUD_tx_state_year_variability/data/private/metrics_distribution_by_state.png",p, width=8,height=4)
