#' Download and Process the Latest Reptile Database Checklist
#'
#' @description
#' Automatically checks for the latest available checklist dataset on
#' \href{http://www.reptile-database.org/}{The Reptile Database}, downloads the
#' raw Excel file, cleans and processes it using \code{\link{clean_reptile_data}},
#' and caches the result for fast future retrieval.
#'
#' @param cache Logical. If \code{TRUE} (default), caches the processed dataset locally
#'   so subsequent calls do not re-download the same dataset version.
#' @param force Logical. If \code{TRUE}, forces re-downloading and re-processing
#'   even if a cached version already exists. Default is \code{FALSE}.
#' @param cache_dir Character string specifying custom cache directory. If \code{NULL}
#'   (default), uses \code{tools::R_user_dir("reptiledb.data", "cache")}.
#' @param silent Logical. If \code{TRUE}, suppresses informative console messages.
#'   Default is \code{FALSE}.
#'
#' @return A cleaned tibble with taxonomic and nomenclatural information.
#'
#' @examples
#' \donttest{
#' # Fetch the latest dataset online
#' reptile_data <- fetch_latest_reptile_data()
#' head(reptile_data)
#' }
#'
#' @export
fetch_latest_reptile_data <- function(cache = TRUE,
                                      force = FALSE,
                                      cache_dir = NULL,
                                      silent = FALSE) {

  stopifnot(
    "cache must be TRUE or FALSE" = is.logical(cache) && length(cache) == 1,
    "force must be TRUE or FALSE" = is.logical(force) && length(force) == 1,
    "silent must be TRUE or FALSE" = is.logical(silent) && length(silent) == 1
  )

  # Determine latest remote file info
  if (!silent) {
    cli::cli_alert_info("Checking for the latest data on The Reptile Database...")
  }

  remote_info <- tryCatch(
    get_latest_reptile_download(return_info = TRUE),
    error = function(e) {
      stop("Unable to fetch remote dataset info: ", e$message, call. = FALSE)
    }
  )

  if (is.null(remote_info) || is.null(remote_info$url)) {
    stop("Could not find download URL on The Reptile Database server.", call. = FALSE)
  }

  # Setup cache paths
  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("reptiledb.data", which = "cache")
  }

  cached_filename <- paste0(tools::file_path_sans_ext(remote_info$filename), ".rds")
  cached_filepath <- file.path(cache_dir, cached_filename)

  # Check if cache hit exists
  if (cache && !force && file.exists(cached_filepath)) {
    if (!silent) {
      cli::cli_alert_success("Loading cached dataset: {.file {cached_filename}}")
    }
    return(readRDS(cached_filepath))
  }

  # Download raw file
  if (!silent) {
    cli::cli_alert_info("Downloading latest dataset from: {.url {remote_info$url}}")
  }

  tmp_ext <- paste0(".", tools::file_ext(remote_info$filename))
  tmp_file <- tempfile(fileext = tmp_ext)
  on.exit(unlink(tmp_file), add = TRUE)

  dl_status <- httr::GET(
    remote_info$url,
    httr::write_disk(tmp_file, overwrite = TRUE),
    httr::user_agent("Mozilla/5.0 (R package reptiledb.data)")
  )

  if (httr::http_error(dl_status)) {
    stop("Failed to download dataset. HTTP status: ", httr::status_code(dl_status), call. = FALSE)
  }

  # Read and process data
  if (!silent) {
    cli::cli_alert_info("Processing taxonomic dataset...")
  }

  raw_data <- readxl::read_excel(tmp_file, sheet = 1)
  processed_data <- clean_reptile_data(raw_data)

  # Save to cache if requested
  if (cache) {
    if (!dir.exists(cache_dir)) {
      dir.create(cache_dir, recursive = TRUE)
    }
    saveRDS(processed_data, file = cached_filepath)
    if (!silent) {
      cli::cli_alert_success("Dataset saved to cache: {.path {cached_filepath}}")
    }
  }

  return(processed_data)
}
