library(testthat)
library(reptiledb.data)

test_that("clean_reptile_data validates input data frame", {
  expect_error(clean_reptile_data("not a dataframe"), "Input 'data' must be a data frame")
  expect_error(clean_reptile_data(data.frame(x = 1)), "Expected 8 columns")
})

test_that("clean_reptile_data cleans dummy reptile data frame correctly", {
  dummy_df <- data.frame(
    type_species = c("sp1", "sp2"),
    species = c("Anolis carolinensis", "Iguana iguana"),
    author = c("Voigt, 1832", "Linnaeus, 1758"),
    subspecies = c("Anolis carolinensis carolinensis Voigt 1832", "Iguana iguana iguana Linnaeus 1758"),
    order = c("Sauria", "Sauria"),
    family = c("Dactyloidae", "Iguanidae"),
    change = c(NA, NA),
    rdb_sp_id = c(101, 102),
    stringsAsFactors = FALSE
  )

  cleaned <- clean_reptile_data(dummy_df)

  expect_s3_class(cleaned, "data.frame")
  expect_equal(ncol(cleaned), 13)
  expect_true("genus" %in% names(cleaned))
  expect_true("epithet" %in% names(cleaned))
  expect_equal(as.character(cleaned$genus[1]), "Anolis")
  expect_equal(as.character(cleaned$epithet[1]), "carolinensis")
})

test_that("fetch_latest_reptile_data input validation works", {
  expect_error(fetch_latest_reptile_data(cache = "invalid"), "cache must be TRUE or FALSE")
  expect_error(fetch_latest_reptile_data(force = "invalid"), "force must be TRUE or FALSE")
  expect_error(fetch_latest_reptile_data(silent = "invalid"), "silent must be TRUE or FALSE")
})
