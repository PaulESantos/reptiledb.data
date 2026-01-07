library(tidyverse)
ruta <- "data-raw\\reptile_checklist_2025_09.xlsx"
data <- readxl::read_excel(ruta,
                           sheet = 1)

data <- data |>
    purrr::set_names(c("type_species",
                       "species",
                       "author",
                       "subspecies",
                       "order",
                       "family",
                       "change",
                       "rdb_sp_id"))


# Main data processing pipeline
reptiledb_092025 <-  data |>
  # Separate multiline entries with safe handling
  tidyr::separate_rows(subspecies, sep = "\r\n") |>
    # Remove control characters (except \v which was already used)
  dplyr::mutate(
    subspecies = stringr::str_replace_all(subspecies, "[[:cntrl:]]", ""),
    subspecies = stringr::str_squish(subspecies)
  ) |>
    # Flag for nomenclature changes (names in parentheses)
  dplyr::mutate(
    subspecies = as.character(subspecies),
    nomenclature_change = ifelse(stringr::str_detect(subspecies,
                                                     "\\(.*\\)"),
                                 TRUE,
                                 FALSE)
  ) |>
    # Clean subspecies text
  dplyr::mutate(
    subspecies = stringr::str_trim(stringr::str_replace_all(subspecies,
                                                            "\\r", "")),
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
  dplyr::mutate(author = stringr::str_remove_all(author,
                                                 "\\(|\\)")) |>
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
      stringr::str_to_title(),
    subspecies_name_author = stringr::str_trim(subspecies_name_author) |>
      stringr::str_to_title(),
    subspecie_author_info = stringr::str_trim(subspecie_author_info) |>
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

reptiledb_092025 |>
 usethis::use_data(compress = "xz",
                   overwrite = TRUE)
