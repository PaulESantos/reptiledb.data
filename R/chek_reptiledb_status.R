#' Check if reptile database data needs updating based on date comparison
#'
#' This function checks if the local reptile database data is up-to-date by
#' comparing the date extracted from the local dataset name with the date
#' from the latest available file on The Reptile Database website.
#'
#' @param silent Logical. If TRUE, suppresses messages and only returns results.
#'   Default is FALSE.
#' @param check_connection Logical. If TRUE, checks internet connection before
#'   attempting to access online data. Default is TRUE.
#'
#' @return A list containing the following elements:
#'   \describe{
#'     \item{update_needed}{Logical. TRUE if an update is needed, FALSE otherwise}
#'     \item{local_info}{List. Information about the local dataset}
#'     \item{remote_info}{List. Information about the remote dataset}
#'     \item{message}{Character. Status message describing the comparison result}
#'     \item{recommendation}{Character. Recommendation for user action}
#'     \item{local_date}{Character. Date of local data in YYYY-MM-DD format}
#'     \item{remote_date}{Character. Date of remote data in YYYY-MM-DD format (if available)}
#'     \item{remote_filename}{Character. Filename of the remote file (if available)}
#'     \item{days_difference}{Numeric. Number of days difference between local and remote data (if both dates available)}
#'   }
#'   If an error occurs or internet connection is not available, only the message
#'   element will contain relevant error information.
#'
#' @examples
#' \donttest{
#' # Silent check (no messages) - requires internet connection
#' update_status <- check_data_update(silent = TRUE)
#'
#' # Verbose check with connection verification
#' update_status <- check_data_update(silent = FALSE, check_connection = TRUE)
#'
#' # Check without internet connection verification
#' update_status <- check_data_update(check_connection = FALSE)
#' }
#'
#' @export
check_data_update <- function(silent = FALSE,
                              check_connection = TRUE) {

  if (!is.logical(silent) || length(silent) != 1) {
    stop("silent must be a single logical value")
  }

  if (!is.logical(check_connection) || length(check_connection) != 1) {
    stop("check_connection must be a single logical value")
  }

  # Initialize result list
  result <- list(
    update_needed = FALSE,
    local_info = list(),
    remote_info = list(),
    message = "",
    recommendation = ""
  )

  # Extract date from local dataset name
  local_date <- "2025-05-01"
  result$local_date <- local_date

  # Check internet connection if requested
  if (check_connection) {
    if (!check_internet_connection()) {
      result$message <- "No internet connection available. Cannot check for updates."
      if (!silent) warning(result$message)
      return(result)
    }
  }

  # Get latest remote file information
  tryCatch({
    # Get latest download URL
    remote_url <- get_latest_reptile_download(return_info = FALSE)

    if (is.null(remote_url)) {
      result$message <- "Could not retrieve latest file information from The Reptile Database."
      if (!silent) warning(result$message)
      return(result)
    }

    # Extract filename from URL
    remote_filename <- basename(remote_url)
    result$remote_filename <- remote_filename

    # Extract date from remote filename (pattern: reptile_checklist_YYYY_MM.xlsx)
    remote_date <- extract_date_from_name(remote_filename, "remote")

    result$remote_date <- format(as.Date(remote_date), "%Y-%m-%d")

    # Compare dates if both are available
    if (!is.null(local_date) && !is.null(remote_date)) {
      days_diff <- as.numeric(as.Date(remote_date) - as.Date(local_date))
      result$days_difference <- days_diff

      if (remote_date == local_date) {
        result$message <- sprintf(
          "Your data is up to date. Local and remote versions match."
        )
        result$update_needed <- FALSE
      } else if (local_date < remote_date) {
        result$message <- "Your data is outdated. Local and remote versions differ."
        result$update_needed <- TRUE
        result$recommendation <- "Visit http://www.reptile-database.org/data/ to download the latest data."
      } else {
        result$message <- "Your local data is newer than the remote version."
        result$update_needed <- FALSE
      }
    } else {
      result$message <- "Could not extract dates for comparison from filenames."
    }

    if (!silent) {
      message(result$message)
      if (result$update_needed) {
        message("Visit http://www.reptile-database.org/data/ to download the latest data.")
      }
    }

  }, error = function(e) {
    result$message <- paste("Error checking for updates:", e$message)
    if (!silent) warning(result$message)
  })

  return(result)
}

#' Extract date from dataset name or filename
#'
#' @param name Dataset name or filename
#' @param type Type of name ("local" or "remote")
#'
#' @return A Date object representing the extracted date, or NULL if extraction fails.
#'   For local datasets, expects pattern "reptiledb_MMYYYY" (e.g., reptiledb_012025).
#'   For remote files, expects pattern "reptile_checklist_YYYY_MM.xlsx".
#'
extract_date_from_name <- function(name, type = "local") {
  if (is.null(name) || is.na(name)) return(NULL)

  if (type == "local") {
    # Pattern for local dataset: reptiledb_MMYYYY (e.g., reptiledb_012025)
    date_match <- stringr::str_extract(name, "\\d{6}")

    if (!is.na(date_match) && nchar(date_match) == 6) {
      month <- substr(date_match, 1, 2)
      year <- paste0("20", substr(date_match, 3, 6))

      # Validate month
      if (as.numeric(month) >= 1 && as.numeric(month) <= 12) {
        return(as.Date(paste(year, month, "01", sep = "-")))
      }
    }
  } else if (type == "remote") {
    # Pattern for remote file: reptile_checklist_YYYY_MM.xlsx
    date_pattern <- "reptile_checklist_(\\d{4})_(\\d{2})\\."
    date_match <- stringr::str_match(name, date_pattern)

    if (!is.na(date_match[1])) {
      year <- date_match[2]
      month <- date_match[3]

      # Validate month
      if (as.numeric(month) >= 1 && as.numeric(month) <= 12) {
        return(as.Date(paste(year, month, "01", sep = "-")))
      }
    }
  }

  return(NULL)
}

#' Check Internet Connection
#'
#' Helper function to check if internet connection is available
#'
#' @return Logical. TRUE if internet is available, FALSE otherwise.
#'
#' @keywords internal
check_internet_connection <- function() {
  tryCatch(class(httr::GET("http://www.google.com/")) == "response",
           error = function(e) {
             return(FALSE)
           }
  )
}

#' Get Latest Reptile Database Download Link
#'
#' This function retrieves the most recent download link for reptile database files
#' from the Reptile Database website. It searches for files from the current year
#' first, and if none are found, searches for files from the previous year.
#'
#' @param base_url Character string. The base URL of the reptile database data page.
#'   Default is "http://www.reptile-database.org/data/".
#' @param current_year Numeric. The current year to search for files.
#'   Default is the current system year.
#' @param file_types Character vector. File extensions to search for.
#'   Default is c("xls", "xlsx", "zip").
#' @param return_info Logical. If TRUE, returns a list with detailed information
#'   about the found file. If FALSE, returns only the URL. Default is FALSE.
#'
#' @return If \code{return_info = FALSE}, returns a character string with the URL
#'   of the most recent file, or NULL if no suitable file is found.
#'   If \code{return_info = TRUE}, returns a list containing:
#'   \describe{
#'     \item{url}{Character. The complete URL of the file}
#'     \item{filename}{Character. The name of the file}
#'     \item{file_type}{Character. The file extension}
#'     \item{extraction_date}{Date. The date when the link was extracted}
#'     \item{source_page}{Character. The source webpage URL}
#'   }
#'   Returns NULL if no suitable file is found or if an error occurs during web scraping.
#'
#' @details The function performs web scraping on the specified URL to find
#'   download links. It prioritizes files from the current year, but will fall
#'   back to the previous year if no current year files are available.
#'
#'   The function requires the following packages: rvest, dplyr, and stringr.
#'   These packages must be installed before using this function.
#'
#' @examples
#' \donttest{
#' # Get just the URL - requires internet connection
#' url <- get_latest_reptile_download()
#'
#' # Get detailed information
#' info <- get_latest_reptile_download(return_info = TRUE)
#'
#' # Search for specific file types
#' zip_url <- get_latest_reptile_download(file_types = "zip")
#'
#' # Search for files from a specific year
#' url_2024 <- get_latest_reptile_download(current_year = 2024)
#' }
#'
#' @seealso \url{http://www.reptile-database.org/} for more information about
#'   the Reptile Database.
#'
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom stringr str_detect str_extract str_starts
#' @importFrom tibble tibble
#'
#' @export
get_latest_reptile_download <- function(base_url = "http://www.reptile-database.org/data/",
                                        current_year = as.numeric(format(Sys.Date(), "%Y")),
                                        file_types = c("xls", "xlsx", "zip"),
                                        return_info = FALSE) {

  # Input validation
  if (!is.character(base_url) || length(base_url) != 1) {
    stop("base_url must be a single character string")
  }

  if (!is.numeric(current_year) || length(current_year) != 1) {
    stop("current_year must be a single numeric value")
  }

  if (!is.character(file_types) || length(file_types) == 0) {
    stop("file_types must be a character vector with at least one element")
  }

  if (!is.logical(return_info) || length(return_info) != 1) {
    stop("return_info must be a single logical value")
  }

  tryCatch({
    # Read the webpage
    page <- rvest::read_html(base_url)

    # Extract all links
    enlaces <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    # Filter download links for specified file types
    file_pattern <- paste0("\\.(", paste(file_types, collapse = "|"), ")$")
    enlaces_descarga <- enlaces[!is.na(enlaces)]
    enlaces_descarga <- enlaces_descarga[stringr::str_detect(enlaces_descarga,
                                                             file_pattern)]

    # Create complete URLs
    enlaces_completos <- ifelse(
      stringr::str_starts(enlaces_descarga, "http"),
      enlaces_descarga,
      paste0(base_url, enlaces_descarga)
    )

    # Create dataframe with link information
    df_enlaces <- tibble::tibble(
      enlace = enlaces_completos,
      tipo_archivo = stringr::str_extract(enlaces_completos, file_pattern),
      nombre_archivo = basename(enlaces_completos)
    )

    # Search for the most recent link (prioritize current year)
    year_pattern <- paste0(current_year, ".*\\.(", paste(file_types,
                                                         collapse = "|"), ")$")
    all_links <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href")

    current_year_links <- all_links[stringr::str_detect(all_links,
                                                        year_pattern)]
    enlace_reciente <- current_year_links[1]

    # If no current year file found, try previous year
    if (is.na(enlace_reciente)) {
      prev_year_pattern <- paste0((current_year - 1), ".*\\.(", paste(file_types,
                                                                      collapse = "|"), ")$")
      prev_year_links <- all_links[stringr::str_detect(all_links,
                                                       prev_year_pattern)]
      enlace_reciente <- prev_year_links[1]
    }

    # Process the found link
    if (!is.na(enlace_reciente)) {
      # Convert to absolute URL if needed
      if (!stringr::str_starts(enlace_reciente, "http")) {
        enlace_reciente <- paste0(base_url, enlace_reciente)
      }

      # Return based on return_info parameter
      if (return_info) {
        return(list(
          url = enlace_reciente,
          filename = basename(enlace_reciente),
          file_type = stringr::str_extract(enlace_reciente, file_pattern),
          extraction_date = Sys.Date(),
          source_page = base_url
        ))
      } else {
        return(enlace_reciente)
      }
    } else {
      warning("No recent download link found for the specified criteria")
      return(NULL)
    }

  }, error = function(e) {
    stop(paste("Error accessing the webpage:", e$message))
  })
}
