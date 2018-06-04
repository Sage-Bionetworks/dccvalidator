context("test-check-ids.R")

test_that("check_indiv_ids returns list of empty elements for valid metadata", {
  expected_result <- list(
    missing_from_assay = character(0),
    missing_from_individual = character(0)
  )
  expect_equal(
    check_indiv_ids(valid_individual, valid_assay),
    expected_result
  )
})

test_that("check_indiv_ids returns list of missing IDs for invalid metadata", {
  expected_result <- list(
    missing_from_assay = character(0),
    missing_from_individual = "ABC"
  )
  expect_equal(
    check_indiv_ids(invalid_individual, invalid_assay),
    expected_result
  )
})

test_that("check_indiv_ids throws error if column is missing", {
  individual <- data.frame(x = 1:10, y = 1:10)
  assay <- data.frame(x = 1:5, y = 1:5)

  expect_error(check_indiv_ids(individual, assay))
})
