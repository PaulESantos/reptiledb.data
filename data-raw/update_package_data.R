# Maintainer Script: Automatically Download and Update Package Datasets
# Usage: Rscript data-raw/update_package_data.R [--force]

suppressPackageStartupMessages({
  library(httr)
  library(readxl)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tidyr)
  library(usethis)
  library(cli)
})

# Source internal cleaning and update check functions if package is not installed
source("R/clean_reptile_data.R")
source("R/chek_reptiledb_status.R")

args <- commandArgs(trailingOnly = TRUE)
force_update <- "--force" %in% args

cli::cli_h1("Reptile Database Package Data Update Script")

# Step 1: Get remote data info
remote_info <- tryCatch(
  get_latest_reptile_download(return_info = TRUE),
  error = function(e) {
    cli::cli_alert_danger("Failed to check remote website: {e$message}")
    NULL
  }
)

if (is.null(remote_info)) {
  cli::cli_alert_danger("Aborting update due to missing remote information.")
  quit(status = 1)
}

filename <- remote_info$filename
remote_date <- extract_date_from_name(filename, "remote")

if (is.null(remote_date)) {
  cli::cli_alert_danger("Could not parse date from filename: {filename}")
  quit(status = 1)
}

mmyyyy <- format(remote_date, "%m%Y")
dataset_name <- paste0("reptiledb_", mmyyyy)
rda_path <- file.path("data", paste0(dataset_name, ".rda"))

cli::cli_alert_info("Remote dataset found: {.field {filename}} (Parsed date: {.val {remote_date}})")
cli::cli_alert_info("Target object name: {.field {dataset_name}}")

if (file.exists(rda_path) && !force_update) {
  cli::cli_alert_success("Dataset {.field {dataset_name}} already exists in data/. No update needed.")
  quit(status = 0)
}

# Step 2: Download Excel file to data-raw/
raw_excel_path <- file.path("data-raw", filename)
cli::cli_alert_info("Downloading raw file to {.path {raw_excel_path}}...")

dl_res <- httr::GET(
  remote_info$url,
  httr::write_disk(raw_excel_path, overwrite = TRUE),
  httr::user_agent("Mozilla/5.0 (R package maintainer script)")
)

if (httr::http_error(dl_res)) {
  cli::cli_alert_danger("Download failed with HTTP status: {httr::status_code(dl_res)}")
  quit(status = 1)
}

# Step 3: Clean and process dataset
cli::cli_alert_info("Cleaning raw reptile checklist data...")
raw_df <- readxl::read_excel(raw_excel_path, sheet = 1)
processed_df <- clean_reptile_data(raw_df)

# Step 4: Save dataset to data/
cli::cli_alert_info("Saving dataset {.field {dataset_name}} to data/...")
assign(dataset_name, processed_df)

eval(substitute(
  usethis::use_data(OBJ, compress = "xz", overwrite = TRUE),
  list(OBJ = as.name(dataset_name))
))

# Step 5: Document dataset in R/reptiledb.R if missing
reptiledb_r_file <- file.path("R", "reptiledb.R")
if (file.exists(reptiledb_r_file)) {
  current_content <- readLines(reptiledb_r_file, warn = FALSE)
  if (!any(grepl(dataset_name, current_content, fixed = TRUE))) {
    cli::cli_alert_info("Appending Roxygen documentation for {.field {dataset_name}} to R/reptiledb.R...")
    month_name <- format(remote_date, "%B %Y")
    doc_block <- sprintf('

#\' Reptile Checklist with Subspecies Information - %s
#\'
#\' A comprehensive dataset extracted from \\href{http://www.reptile-database.org/}{The Reptile Database}
#\' containing taxonomic and nomenclatural information for reptile species and their subspecies.
#\' Each row corresponds to a species–subspecies combination (or a species without subspecies),
#\' with fields for authorship, year of description, and identifiers used by The Reptile Database.
#\'
#\' @format A tibble with 13 columns.
#\' @source \\url{http://www.reptile-database.org/}
#\'
"%s"
', month_name, dataset_name)
    cat(doc_block, file = reptiledb_r_file, append = TRUE)
  }
}

# Step 6: Document package
if (requireNamespace("devtools", quietly = TRUE)) {
  cli::cli_alert_info("Updating package documentation...")
  devtools::document()
}

cli::cli_alert_success("Successfully updated package dataset to {.field {dataset_name}}!")

