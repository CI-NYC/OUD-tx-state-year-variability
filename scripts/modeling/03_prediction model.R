
library(glue)
library(tictoc)
library(dplyr)
library(mlr3)
library(mlr3superlearner)

source("~/medicaid/OUD_tx_state_year_variability/R/helpers.R")


race_groups <- c("race_white", 
                 "race_multi_na",
                 "race_black", 
                 "race_hispanic",
                 "race_aian_hpi",
                 "race_asian"
                 )

outcomes <- c("initiation_metric",
              "engagement_metric",
              "retention_metric",
              "counseling_metric")

cohort <- load_data("hillary_cohort_merged_metrics.fst", drv_root)


fit <- readRDS(file.path(drv_root, "mlr3_results.rds"))
predictions <- data.frame()


tic(paste0("Year: ", my_year, ", Pain group: ", pain))
for (race in race_groups){

  # preallocating dataframe for storing the predicted outcomes for each race
  predictions_censoring <- data.frame(matrix(NA, nrow = nrow(analysis_cohort), ncol = 5))
  colnames(predictions_censoring) <-
    c(
      "initiation_metric",
      "engagement_metric",
      "retention_metric",
      "counseling_metric"
      # "remained_enrolled"
      )
  # predictions_censoring <- predictions_censoring


  for (i in 1:4){
    predictions_censoring[,i+1] <- predict(fit[[i]]$fit, analysis_cohort)
  }

  predictions <- rbind(predictions, predictions_censoring)
}
toc()
saveRDS(predictions_pain_group, file.path(save_dir, my_year, glue("{my_year}_{pain}_mlr3_predictions.rds")))


