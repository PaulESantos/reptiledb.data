#' Reptile Checklist with Subspecies Information - January 2025
#'
#' A comprehensive dataset extracted from \href{http://www.reptile-database.org/}{The Reptile Database}
#' containing taxonomic and nomenclatural information for reptile species and their subspecies.
#' Each row corresponds to a species–subspecies combination (or a species without subspecies),
#' with fields for authorship, year of description, and identifiers used by The Reptile Database.
#'
#' @format A tibble with 14,539 rows and 13 columns:
#' \describe{
#'   \item{order}{Taxonomic order of the reptile (factor; e.g., \code{"Sauria"}).}
#'   \item{family}{Taxonomic family (factor; e.g., \code{"Scincidae"}).}
#'   \item{genus}{Genus name (factor).}
#'   \item{epithet}{Species epithet (second part of the species name; factor).}
#'   \item{species}{Full species name (genus + epithet; character).}
#'   \item{species_author}{Primary author(s) of the species name (factor).}
#'   \item{species_name_year}{Year in which the species was described (character).}
#'   \item{subspecies_name}{Epithet of the subspecies, when available (character; \code{NA} if no subspecies).}
#'   \item{subspecie_author_info}{Full author citation of the subspecies (character; \code{NA} if no subspecies).}
#'   \item{subspecies_name_author}{Author(s) of the subspecies name (character; \code{NA} if no subspecies).}
#'   \item{subspecies_year}{Year in which the subspecies was described (character; \code{NA} if no subspecies).}
#'   \item{change}{Optional free-text description of taxonomic or nomenclatural changes (factor; \code{NA} when no change is recorded).}
#'   \item{rdb_sp_id}{Numeric identifier assigned to the species by The Reptile Database (double).}
#' }
#'
#' @details
#' The dataset is a structured export of reptile taxonomy information from The Reptile Database.
#' Species with multiple subspecies appear in multiple rows (one per subspecies), sharing the same
#' \code{species} and \code{rdb_sp_id} values. Species without recognized subspecies appear in a single
#' row with all subspecies-related fields set to \code{NA}.
#'
#' This object is part of the \pkg{reptiledb.data} package and is intended to support filtering,
#' analysis, and visualization of reptile diversity and nomenclatural history across orders, families,
#' genera, and species.
#'
#' @source \url{http://www.reptile-database.org/}
#'
"reptiledb_012025"

#' Reptile Checklist with Subspecies Information - September 2025
#'
#' A comprehensive dataset extracted from \href{http://www.reptile-database.org/}{The Reptile Database}
#' containing taxonomic and nomenclatural information for reptile species and their subspecies.
#' Each row corresponds to a species–subspecies combination (or a species without subspecies),
#' with fields for authorship, year of description, and identifiers used by The Reptile Database.
#'
#' This object corresponds to the September 2025 snapshot of the checklist.
#'
#' @format A tibble with 14,585 rows and 13 columns:
#' \describe{
#'   \item{order}{Taxonomic order of the reptile (factor; e.g., \code{"Sauria"}).}
#'   \item{family}{Taxonomic family (factor; e.g., \code{"Scincidae"}).}
#'   \item{genus}{Genus name (factor).}
#'   \item{epithet}{Species epithet (second part of the species name; factor).}
#'   \item{species}{Full species name (genus + epithet; character).}
#'   \item{species_author}{Primary author(s) of the species name (factor).}
#'   \item{species_name_year}{Year in which the species was described (character).}
#'   \item{subspecies_name}{Epithet of the subspecies, when available (character; \code{NA} if no subspecies).}
#'   \item{subspecie_author_info}{Full author citation of the subspecies (character; \code{NA} if no subspecies).}
#'   \item{subspecies_name_author}{Author(s) of the subspecies name (character; \code{NA} if no subspecies).}
#'   \item{subspecies_year}{Year in which the subspecies was described (character; \code{NA} if no subspecies).}
#'   \item{change}{Optional free-text description of taxonomic or nomenclatural changes (factor; \code{NA} when no change is recorded).}
#'   \item{rdb_sp_id}{Numeric identifier assigned to the species by The Reptile Database (double).}
#' }
#'
#' @details
#' The dataset is a structured export of reptile taxonomy information from The Reptile Database.
#' Species with multiple subspecies appear in multiple rows (one per subspecies), sharing the same
#' \code{species} and \code{rdb_sp_id} values. Species without recognized subspecies appear in a single
#' row with all subspecies-related fields set to \code{NA}.
#'
#' This object is part of the \pkg{reptiledb.data} package and is intended to support filtering,
#' analysis, and visualization of reptile diversity and nomenclatural history across orders, families,
#' genera, and species.
#'
#' @source \url{http://www.reptile-database.org/}
#'
"reptiledb_092025"
