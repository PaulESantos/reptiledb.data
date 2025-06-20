# Tests for reptiledb_012025 dataset
library(testthat)
library(reptiledb.data)

test_that("reptiledb_012025 dataset exists and loads correctly", {
  expect_true(exists("reptiledb_012025"))
  expect_s3_class(reptiledb_012025, "data.frame")
  expect_s3_class(reptiledb_012025, "tbl_df")
})

test_that("reptiledb_012025 has correct structure", {
  # Test number of columns
  expect_equal(ncol(reptiledb_012025), 13)

  # Test number of rows (approximately)
  expect_gt(nrow(reptiledb_012025), 14000)
  expect_lt(nrow(reptiledb_012025), 20000)  # reasonable upper bound

  # Test expected column names
  expected_cols <- c("order", "family", "genus", "epithet", "species",
                     "species_author", "species_name_year", "subspecies_name",
                     "subspecie_author_info", "subspecies_name_author",
                     "subspecies_year", "change", "rdb_sp_id")

  expect_equal(colnames(reptiledb_012025), expected_cols)
})

test_that("reptiledb_012025 has correct column types", {
  # Factor columns
  expect_s3_class(reptiledb_012025$order, "factor")
  expect_s3_class(reptiledb_012025$family, "factor")
  expect_s3_class(reptiledb_012025$genus, "factor")
  expect_s3_class(reptiledb_012025$epithet, "factor")
  expect_s3_class(reptiledb_012025$species_author, "factor")
  expect_s3_class(reptiledb_012025$change, "factor")

  # Character columns
  expect_type(reptiledb_012025$species, "character")
  expect_type(reptiledb_012025$species_name_year, "character")
  expect_type(reptiledb_012025$subspecies_name, "character")
  expect_type(reptiledb_012025$subspecie_author_info, "character")
  expect_type(reptiledb_012025$subspecies_name_author, "character")
  expect_type(reptiledb_012025$subspecies_year, "character")

  # Numeric columns
  expect_type(reptiledb_012025$rdb_sp_id, "double")
})

test_that("reptiledb_012025 key columns have no missing values", {
  # Essential taxonomic columns should not be completely empty
  expect_true(all(!is.na(reptiledb_012025$order)))
  expect_true(all(!is.na(reptiledb_012025$family)))
  expect_true(all(!is.na(reptiledb_012025$genus)))
  expect_true(all(!is.na(reptiledb_012025$epithet)))
  expect_true(all(!is.na(reptiledb_012025$species)))
  expect_true(all(!is.na(reptiledb_012025$rdb_sp_id)))
})

test_that("reptiledb_012025 species column format is correct", {
  # Species should be in format "Genus epithet"
  species_pattern <- "^[A-Z][a-z]+ [a-z]+$"
  species_valid <- grepl(species_pattern, reptiledb_012025$species)

  # Allow for some exceptions but expect most to follow pattern
  valid_percentage <- sum(species_valid) / length(species_valid)
  expect_gt(valid_percentage, 0.95)  # At least 95% should be valid
})



test_that("reptiledb_012025 has expected reptile orders", {
  # Check for major reptile orders
  expected_orders <- c("Sauria", "Serpentes", "Testudines", "Crocodilia")

  actual_orders <- levels(reptiledb_012025$order)

  # At least some of the major orders should be present
  major_orders_present <- sum(expected_orders %in% actual_orders)
  expect_gt(major_orders_present, 2)  # At least 3 major orders
})

test_that("reptiledb_012025 year columns have reasonable values", {
  # Remove NA values for testing
  species_years <- reptiledb_012025$species_name_year[!is.na(reptiledb_012025$species_name_year)]
  subspecies_years <- reptiledb_012025$subspecies_year[!is.na(reptiledb_012025$subspecies_year)]

  if(length(species_years) > 0) {
    # Extract numeric years (assuming format like "1901", "1832", etc.)
    species_year_nums <- as.numeric(species_years)
    species_year_nums <- species_year_nums[!is.na(species_year_nums)]

    if(length(species_year_nums) > 0) {
      expect_true(all(species_year_nums >= 1750))  # Linnaean nomenclature era
      expect_true(all(species_year_nums <= 2025))  # Current year
    }
  }

  if(length(subspecies_years) > 0) {
    subspecies_year_nums <- as.numeric(subspecies_years)
    subspecies_year_nums <- subspecies_year_nums[!is.na(subspecies_year_nums)]

    if(length(subspecies_year_nums) > 0) {
      expect_true(all(subspecies_year_nums >= 1750))
      expect_true(all(subspecies_year_nums <= 2025))
    }
  }
})



test_that("reptiledb_012025 subspecies data is consistent", {
  # When subspecies_name is present, other subspecies fields should also be present
  has_subspecies <- !is.na(reptiledb_012025$subspecies_name)

  if(sum(has_subspecies) > 0) {
    subspecies_data <- reptiledb_012025[has_subspecies, ]

    # If subspecies name exists, at least author info should exist
    expect_true(sum(!is.na(subspecies_data$subspecie_author_info)) > 0 |
                  sum(!is.na(subspecies_data$subspecies_name_author)) > 0)
  }
})

test_that("reptiledb_012025 family names follow conventions", {
  # Reptile family names typically end in -idae
  family_names <- levels(reptiledb_012025$family)
  idae_families <- sum(grepl("idae$", family_names))

  # Most families should follow this convention
  expect_gt(idae_families / length(family_names), 0.7)  # At least 70%
})

test_that("reptiledb_012025 data completeness", {
  # Calculate completeness for each column
  completeness <- sapply(reptiledb_012025, function(x) sum(!is.na(x)) / length(x))

  # Core taxonomic fields should be highly complete
  expect_gt(completeness[["order"]], 0.99)
  expect_gt(completeness[["family"]], 0.99)
  expect_gt(completeness[["genus"]], 0.99)
  expect_gt(completeness[["epithet"]], 0.99)
  expect_gt(completeness[["species"]], 0.99)

  # Subspecies fields are expected to have many NAs (not all species have subspecies)
  expect_lt(completeness[["subspecies_name"]], 0.5)  # Less than 50% complete is normal
})

test_that("reptiledb_012025 has reasonable data distribution", {
  # Test that we have multiple families represented
  expect_gt(length(levels(reptiledb_012025$family)), 50)

  # Test that we have multiple genera
  expect_gt(length(levels(reptiledb_012025$genus)), 1000)

  # Test that Scincidae family is present (as shown in example)
  expect_true("Scincidae" %in% levels(reptiledb_012025$family))

  # Test that Sauria order is present (as shown in example)
  expect_true("Sauria" %in% levels(reptiledb_012025$order))
})
