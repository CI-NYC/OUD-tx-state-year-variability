# -------------------------------------
# Script: 08_mediator_physical_therapy.R
# Author: Nick Williams
# Updated:
# Purpose: Creates an indicator variable for whether or not an observation in
#   the analysis cohort had a claim for physical therapy
#   during the mediator period.
# Notes:
# -------------------------------------

library(arrow)
library(dplyr)
library(lubridate)
library(data.table)
library(yaml)

source("~/medicaid/OUD_tx_state_year_variability//R/helpers.R")

iph <- open_iph()
otl <- open_otl()

# Read in cohort and dates
cohort <- load_data("hillary_cohort_with_exclusions.fst", drv_root) |>
  filter(exclusion_nov17 == 0)

# Read in CPT, HCPC, and Modifier codes for mediator claims
codes <- read_yaml("~/medicaid/OUD_tx_state_year_variability/data/public/engagement_codes.yml")
mediators <- c("Psychiatric Diagnostic Evaluation",
               "Primary Care",
               "Addiction Medicines"
               )
treatments_df <- data.frame()
for (mediator in mediators) {
  for (code in codes[[mediator]]){
    treatments_df <- rbind(treatments_df, 
                           data.frame(cd = names(code), 
                                      treatment = rep(mediator, length(names(code))))
    )
  }
}

# Filter OTL to claims codes
claims_vars <- c("BENE_ID", "LINE_SRVC_BGN_DT", "LINE_SRVC_END_DT", "LINE_PRCDR_CD_SYS", "LINE_PRCDR_CD")
claims <- select(otl, all_of(claims_vars)) |> 
  filter(LINE_PRCDR_CD %in% treatments_df$cd) |>
  collect()

setDT(claims)
setkey(claims, BENE_ID)

claims[, LINE_SRVC_BGN_DT := fifelse(is.na(LINE_SRVC_BGN_DT), 
                                     LINE_SRVC_END_DT, 
                                     LINE_SRVC_BGN_DT)]

# Inner join with cohort 
claims <- unique(merge(claims, cohort, by = "BENE_ID"))

# Filter to claims within mediator time-frame
claims <- claims[LINE_SRVC_BGN_DT %within% interval(index_dt, 
                                                    index_dt + days(44)), 
                 .(BENE_ID, LINE_SRVC_BGN_DT, index_dt, LINE_PRCDR_CD)]

# Create indicator variable for whether or not a patient had claim in mediator period
# Right join with cohort
claims <- claims[, .(engagement_metric = as.numeric(.N > 0)), by = "BENE_ID"]
claims <- merge(claims, cohort, all.y = TRUE, by = "BENE_ID")

# Convert NAs to 0 for observations in the cohort that didn't have a PT claim
fix <- c("engagement_metric")
claims[, (fix) := lapply(.SD, \(x) fifelse(is.na(x), 0, x)), .SDcols = fix]

# claims <- claims |>
#   left_join(treatments_df, by = c("LINE_PRCDR_CD" = "cd"))

write_data(claims, "engagement_metric.fst", drv_root)
