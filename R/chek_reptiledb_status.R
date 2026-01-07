#' Check if reptile database data needs updating
#'
#' @description
#' Compares the local reptile database version with the latest available
#' version on The Reptile Database website and provides user-friendly feedback.
#'
#' @param version_date Character string indicating the most recent date of the data source.
#' @param silent Logical. If TRUE, suppresses all messages and returns only the
#'   result object. Default is FALSE.
#' @param check_connection Logical. If TRUE, verifies internet connection before
#'   attempting to check for updates. Default is TRUE.
#'
#' @return An S3 object of class "reptile_update_check" with the following components:
#'   \describe{
#'     \item{needs_update}{Logical. TRUE if update is available}
#'     \item{local_date}{Date. Version date of local data}
#'     \item{remote_date}{Date. Version date of remote data (if available)}
#'     \item{days_behind}{Integer. Days between local and remote versions}
#'     \item{download_url}{Character. URL to download latest data}
#'     \item{status}{Character. One of "current", "outdated", "newer", "unknown", "no_connection"}
#'   }
#'
#' @examples
#' \donttest{
#' # Check with user-friendly messages
#' check_data_update()
#'
#' # Silent check for programmatic use
#' result <- check_data_update(silent = TRUE)
#' if (result$needs_update) {
#'   message("Update available!")
#' }
#' }
#'
#' @export
check_data_update <- function(version_date = "2025-09-01",
                              silent = FALSE,
                              check_connection = TRUE) {

  # Input validation
  stopifnot(
    "silent must be TRUE or FALSE" = is.logical(silent) && length(silent) == 1,
    "check_connection must be TRUE or FALSE" = is.logical(check_connection) && length(check_connection) == 1
  )

  # Initialize result
  result <- structure(
    list(
      needs_update = FALSE,
      local_date = as.Date(version_date),
      remote_date = NULL,
      days_behind = NULL,
      download_url = "http://www.reptile-database.org/data/",
      status = "unknown"
    ),
    class = "reptile_update_check"
  )

  # Check internet connection
  if (check_connection && !check_internet_connection()) {
    result$status <- "no_connection"
    if (!silent) {
      print(result)
    }
    return(invisible(result))
  }

  # Get remote information
  remote_info <- tryCatch(
    get_latest_reptile_download(return_info = TRUE),
    error = function(e) {
      if (!silent) {
        cli::cli_alert_danger("Failed to check for updates")
        cli::cli_text("{.emph {e$message}}")
      }
      return(NULL)
    }
  )

  if (is.null(remote_info)) {
    result$status <- "unknown"
    if (!silent) {
      print(result)
    }
    return(invisible(result))
  }

  # Extract and compare dates
  result$remote_date <- extract_date_from_name(remote_info$filename, "remote")

  if (is.null(result$remote_date)) {
    result$status <- "unknown"
    if (!silent) {
      print(result)
    }
    return(invisible(result))
  }

  # Calculate difference
  result$days_behind <- as.integer(result$remote_date - result$local_date)

  # Determine status
  if (result$local_date == result$remote_date) {
    result$status <- "current"
    result$needs_update <- FALSE
  } else if (result$local_date < result$remote_date) {
    result$status <- "outdated"
    result$needs_update <- TRUE
  } else {
    result$status <- "newer"
    result$needs_update <- FALSE
  }

  # Print if not silent
  if (!silent) {
    print(result)
  }

  return(invisible(result))
}

#' Print method for reptile_update_check objects
#'
#' @param x An object of class "reptile_update_check"
#' @param ... Additional arguments (not used)
#'
#' @export
print.reptile_update_check <- function(x, ...) {
  cli::cli_rule("Reptile Database Update Check")
  cli::cli_text("")

  switch(x$status,
         "current" = {
           cli::cli_alert_success("Your database is up to date")
           cli::cli_text("Version: {.field {format(x$local_date, '%B %Y')}}")
         },
         "outdated" = {
           cli::cli_alert_warning("A newer version is available")
           cli::cli_text("")
           cli::cli_dl(c(
             "Your version" = format(x$local_date, "%B %Y"),
             "Latest version" = format(x$remote_date, "%B %Y"),
             "Days behind" = paste(x$days_behind, "days")
           ))
           cli::cli_text("")
           cli::cli_alert_info("Download the latest version:")
           cli::cli_text("{.url {x$download_url}}")
         },
         "newer" = {
           cli::cli_alert_info("Your local version is newer than the remote version")
           cli::cli_text("")
           cli::cli_dl(c(
             "Local" = format(x$local_date, "%B %Y"),
             "Remote" = format(x$remote_date, "%B %Y")
           ))
           cli::cli_text("")
           cli::cli_text("{.emph This is unexpected but not necessarily a problem}")
         },
         "no_connection" = {
           cli::cli_alert_warning("No internet connection available")
           cli::cli_text("Cannot check for updates at this time")
           cli::cli_text("")
           cli::cli_text("Local version: {.field {format(x$local_date, '%B %Y')}}")
         },
         "unknown" = {
           cli::cli_alert_warning("Could not determine update status")
           cli::cli_text("")
           cli::cli_text("Local version: {.field {format(x$local_date, '%B %Y')}}")
           cli::cli_text("Remote version: {.emph could not be retrieved}")
         }
  )

  cli::cli_text("")
  invisible(x)
}

#' Extract date from dataset name or filename
#'
#' @param name Character. Dataset name or filename
#' @param type Character. Type of name ("local" or "remote")
#'
#' @return A Date object or NULL if extraction fails
#'
#' @keywords internal
extract_date_from_name <- function(name, type = "local") {
  if (is.null(name) || is.na(name) || !nzchar(name)) {
    return(NULL)
  }

  result <- if (type == "local") {
    # Pattern: reptiledb_MMYYYY (e.g., reptiledb_012025)
    date_match <- stringr::str_extract(name, "\\d{6}")

    if (!is.na(date_match) && nchar(date_match) == 6) {
      month <- substr(date_match, 1, 2)
      year <- paste0("20", substr(date_match, 3, 6))

      month_num <- as.integer(month)
      if (month_num >= 1 && month_num <= 12) {
        as.Date(sprintf("%s-%s-01", year, month))
      } else {
        NULL
      }
    } else {
      NULL
    }

  } else if (type == "remote") {
    # Pattern: reptile_checklist_YYYY_MM.xlsx
    date_match <- stringr::str_match(name, "reptile_checklist_(\\d{4})_(\\d{2})\\.")

    if (!is.na(date_match[1])) {
      year <- date_match[2]
      month <- date_match[3]

      month_num <- as.integer(month)
      if (month_num >= 1 && month_num <= 12) {
        as.Date(sprintf("%s-%s-01", year, month))
      } else {
        NULL
      }
    } else {
      NULL
    }
  } else {
    NULL
  }

  return(result)
}

#' Check Internet Connection
#'
#' @return Logical. TRUE if internet is available
#'
#' @keywords internal
check_internet_connection <- function() {
  tryCatch(
    {
      response <- httr::GET("https://www.google.com", httr::timeout(5))
      httr::status_code(response) == 200
    },
    error = function(e) FALSE
  )
}

#' Get Latest Reptile Database Download Information
#'
#' @description
#' Retrieves information about the most recent reptile database file
#' available for download from the official website.
#'
#' @param base_url Character. The base URL of the data page.
#'   Default: "http://www.reptile-database.org/data/"
#' @param current_year Numeric. Year to prioritize in search.
#'   Default: current system year
#' @param file_types Character vector. File extensions to search for.
#'   Default: c("xlsx", "xls", "zip")
#' @param return_info Logical. If TRUE, returns detailed information list.
#'   If FALSE, returns only the URL string. Default: FALSE
#'
#' @return
#' If `return_info = FALSE`: Character string with the download URL, or NULL
#'
#' If `return_info = TRUE`: A list with:
#'   \describe{
#'     \item{url}{Complete download URL}
#'     \item{filename}{File name}
#'     \item{file_type}{File extension}
#'     \item{checked_at}{Timestamp of check}
#'     \item{source_page}{Source webpage URL}
#'   }
#'
#' @examples
#' \donttest{
#' # Get download URL
#' url <- get_latest_reptile_download()
#'
#' # Get detailed information
#' info <- get_latest_reptile_download(return_info = TRUE)
#' }
#'
#' @export
get_latest_reptile_download <- function(
    base_url = "http://www.reptile-database.org/data/",
    current_year = as.integer(format(Sys.Date(), "%Y")),
    file_types = c("xlsx", "xls", "zip"),
    return_info = FALSE
) {

  # Input validation
  stopifnot(
    "base_url must be a single character string" =
      is.character(base_url) && length(base_url) == 1,
    "current_year must be a single numeric value" =
      is.numeric(current_year) && length(current_year) == 1,
    "file_types must be a non-empty character vector" =
      is.character(file_types) && length(file_types) > 0,
    "return_info must be TRUE or FALSE" =
      is.logical(return_info) && length(return_info) == 1
  )

  tryCatch({
    # Read webpage
    page <- rvest::read_html(base_url)

    # Extract all links
    all_links <- page |>
      rvest::html_nodes("a") |>
      rvest::html_attr("href") |>
      stats::na.omit()

    # Build file pattern
    file_pattern <- sprintf("\\.((%s))$", paste(file_types, collapse = "|"))

    # Function to find links by year
    find_year_links <- function(year) {
      year_pattern <- sprintf("%d.*%s", year, file_pattern)
      matches <- all_links[stringr::str_detect(all_links, year_pattern)]
      if (length(matches) > 0) matches[1] else NA_character_
    }

    # Try current year, then previous year
    recent_link <- find_year_links(current_year)
    if (is.na(recent_link)) {
      recent_link <- find_year_links(current_year - 1)
    }

    # Process result
    if (is.na(recent_link)) {
      warning("No recent download link found", call. = FALSE)
      return(NULL)
    }

    # Convert to absolute URL
    if (!stringr::str_starts(recent_link, "http")) {
      recent_link <- paste0(base_url, recent_link)
    }

    # Return based on parameter
    if (return_info) {
      list(
        url = recent_link,
        filename = basename(recent_link),
        file_type = stringr::str_extract(recent_link, file_pattern),
        checked_at = Sys.time(),
        source_page = base_url
      )
    } else {
      recent_link
    }

  }, error = function(e) {
    stop("Failed to access ", base_url, ": ", e$message, call. = FALSE)
  })
}
