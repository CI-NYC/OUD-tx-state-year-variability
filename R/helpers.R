# -------------------------------------
# Script: helpers.R
# Author: Nick Williams
# Updated:
# Purpose: Functions for repeat tasks
# Notes:
# -------------------------------------

library(arrow)
library(fst)

drv_root <- "/mnt/general-data/disability/tx_state_year_variability/"

write_data <- function(data, file, dir) {
  write_fst(data, file.path(dir, file))
}

load_data <- function(file, dir) {
  read_fst(file.path(dir, file))
}

#' Open an arrow dataset for the dates files
open_dedts <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFDEDTS_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

#' Open an arrow dataset for the inpatient hospital files
open_iph <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFIPH_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

#' Open an arrow dataset for the other services files
open_oth <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFOTH\\d+_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_otl <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFOTL\\d+_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_rxl <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFRXL_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_rxh <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFRXH_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_demo <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFDEBSE_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_demc <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFDEMC_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}


get_rxcui_status <- function(rx_cui, local_host = FALSE) {
  # https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getRxcuiHistoryStatus.html
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_status(httr::GET(url)))
}

parse_rxcui_status <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$metaData$status
}


# get_remapped_rxcui <- function(rx_cui, local_host = FALSE) {
#   # check_internet()
#   url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
#   check_null(parse_rxcui_remapped(httr::GET(url)))
# }
#
# parse_rxcui_remapped <- function(x) {
#   if (!check_status(x)) return(NA_character_)
#   httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$remappedConcept[[1]]$remappedRxCui
# }


get_quantified_rxcui <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_quantified(httr::GET(url)))
}

parse_rxcui_quantified <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$quantifiedConcept[[1]]$quantifiedRxcui
}


get_scd_rxcui <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  browser()
  check_null(parse_rxcui_scd(httr::GET(url)))
}

parse_rxcui_scd <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$scdConcept$scdConceptRxcui
}

create_url <- function(local, path, ...) {
  query <- list(...)
  url <- list()
  class(url) <- "url"

  url$scheme <- ifelse(local, "http", "https")
  url$hostname <- ifelse(local, "localhost", "rxnav.nlm.nih.gov")
  url$port <- NULL
  if (local) url$port <- "4000"
  url$path <- path

  if (length(query) != 0) {
    url$query <- list()
    for (x in names(query)) {
      url$query[[x]] <- query[[x]]
    }
  }
  httr::build_url(url)
}

check_null <- function(x) {
  if (is.null(x)) return(NA_character_)
  x
}

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

check_common <- function(who) {
  unique(unlist(who))
}
