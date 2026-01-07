#' Clean and Process Reptile Checklist Data
#'
#' This function processes and cleans raw reptile checklist data by extracting
#' taxonomic information, handling nomenclature changes, and standardizing
#' species and subspecies names with their respective authors and years.
#'
#' @param data A data frame containing the raw reptile checklist data with
#'   columns: type_species, species, author, subspecies, order, family, change, rdb_sp_id
#' @param rename_cols Logical, whether to rename columns to standard names
#'   (default: TRUE)
#'
#' @return A cleaned data frame with the following columns:
#'   \describe{
#'     \item{order}{Taxonomic order (factor)}
#'     \item{family}{Taxonomic family (factor)}
#'     \item{genus}{Genus name (factor)}
#'     \item{epithet}{Species epithet (factor)}
#'     \item{species}{Original species name}
#'     \item{species_author}{Species author name (title case)}
#'     \item{species_name_year}{Year of species description}
#'     \item{subspecies_name}{Subspecies name}
#'     \item{subspecie_author_info}{Raw subspecies author information}
#'     \item{subspecies_name_author}{Subspecies author name (title case)}
#'     \item{subspecies_year}{Year of subspecies description}
#'     \item{change}{Change indicator (factor)}
#'     \item{rdb_sp_id}{Reptile Database species ID}
#'   }
#'
#' @details
#' The function performs the following operations:
#' \enumerate{
#'   \item Renames columns to standard names if requested
#'   \item Handles multiline entries by separating rows on vertical tab characters
#'   \item Removes control characters and extra whitespace
#'   \item Identifies nomenclature changes (names in parentheses)
#'   \item Extracts genus, species epithet, and subspecies names using regex
#'   \item Extracts author names and publication years for both species and subspecies
#'   \item Formats author names to title case
#'   \item Converts appropriate columns to factors for memory efficiency
#' }
#'
#'
#' @keywords internal
clean_reptile_data <- function(data, rename_cols = TRUE) {

  # Validate input
  if (!is.data.frame(data)) {
    stop("Input 'data' must be a data frame")
  }

  # Rename columns if requested
  if (rename_cols) {
    if (ncol(data) != 8) {
      stop("Expected 8 columns in input data for renaming")
    }

    data <- data |>
      purrr::set_names(c("type_species",
                         "species",
                         "author",
                         "subspecies",
                         "order",
                         "family",
                         "change",
                         "rdb_sp_id"))
  }

  # Required columns check
  required_cols <- c("species", "author", "subspecies", "order", "family", "change", "rdb_sp_id")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Main data processing pipeline
  processed_data <- data |>
    # Separate multiline entries with safe handling
    tidyr::separate_rows(subspecies, sep = "\v") |>

    # Remove control characters (except \v which was already used)
    dplyr::mutate(
      subspecies = stringr::str_replace_all(subspecies, "[[:cntrl:]]", ""),
      subspecies = stringr::str_squish(subspecies)
    ) |>

    # Flag for nomenclature changes (names in parentheses)
    dplyr::mutate(
      subspecies = as.character(subspecies),
      nomenclature_change = ifelse(stringr::str_detect(subspecies, "\\(.*\\)"), TRUE, FALSE)
    ) |>

    # Clean subspecies text
    dplyr::mutate(
      subspecies = stringr::str_trim(stringr::str_replace_all(subspecies, "\\r", "")),
      subspecies = stringr::str_remove_all(subspecies, "\\(|\\)")
    ) |>

    # Extract components with flexible regex
    tidyr::extract(
      subspecies,
      into = c("genus", "epithet", "subspecies_name", "subspecie_author_info"),
      regex = "([A-Z][a-z]+)\\s+([a-z\\-]+)\\s+([a-z\\-]+)\\s+(.+)",
      remove = FALSE,
      convert = TRUE
    ) |>

    # Handle cases where primary extraction fails
    dplyr::mutate(
      genus = ifelse(is.na(genus),
                     stringr::word(subspecies, 1, 1),
                     genus),
      epithet = ifelse(is.na(epithet),
                       stringr::word(subspecies, 2, 2),
                       epithet),
      subspecies_name = ifelse(is.na(subspecies_name),
                               stringr::word(subspecies, 3, 3),
                               subspecies_name)
    ) |>

    # Clean empty strings
    dplyr::mutate(
      genus = ifelse(nchar(genus) < 1, NA_character_, genus),
      subspecies = ifelse(nchar(subspecies) < 1, NA_character_, subspecies)
    ) |>

    # Fallback to species column for genus and epithet
    dplyr::mutate(
      genus = ifelse(is.na(genus),
                     stringr::word(species, 1, 1),
                     genus),
      epithet = ifelse(is.na(epithet),
                       stringr::word(species, 2, 2),
                       epithet)
    ) |>

    # Extract subspecies year and author
    dplyr::mutate(
      subspecies_year = stringr::str_extract(subspecie_author_info, "[0-9]{4}"),
      subspecies_name_author = stringr::str_trim(
        stringr::str_replace(subspecie_author_info, "[0-9]{4}", "")
      ),
      subspecies_name_author = stringr::str_trim(
        stringr::str_replace_all(subspecies_name_author, "\\s+", " ")
      ),
      subspecies_name_author = stringr::str_to_title(subspecies_name_author)
    ) |>

    # Flag nomenclature changes for species
    dplyr::mutate(
      nomenclature_change_species = ifelse(
        stringr::str_detect(author, "\\(.*\\)"),
        TRUE,
        FALSE
      )
    ) |>

    # Clean author column
    dplyr::mutate(author = stringr::str_remove_all(author, "\\(|\\)")) |>

    # Extract species year and author
    dplyr::mutate(
      species_name_year = stringr::str_extract(author, "[0-9]{4}"),
      species_name_year = dplyr::case_when(
        is.na(species_name_year) & stringr::str_detect(author, "\\([^)]*[0-9]{4}[^)]*\\)") ~
          stringr::str_extract(author, "(?<=\\()[^)]*[0-9]{4}[^)]*(?=\\))"),
        TRUE ~ species_name_year
      ),
      species_name_year = stringr::str_extract(species_name_year, "[0-9]{4}"),
      species_author = stringr::str_remove_all(author, "[0-9]{4}"),
      species_author = stringr::str_remove_all(species_author, "\\(|\\)"),
      species_author = stringr::str_trim(species_author) |>
        stringr::str_to_title()
    ) |>

    # Select and reorganize columns
    dplyr::select(
      order,
      family,
      genus,
      epithet,
      species,
      species_author,
      species_name_year,
      subspecies_name,
      subspecie_author_info,
      subspecies_name_author,
      subspecies_year,
      change,
      rdb_sp_id
    ) |>

    # Convert appropriate columns to factors
    dplyr::mutate(
      dplyr::across(
        .cols = c(order, family, genus, epithet, species_author, change),
        .fns = as.factor
      )
    )

  return(processed_data)
}



usethis::use_data(get_reptiledb, overwrite = TRUE)
