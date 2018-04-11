context("test-check-ids.R")

test_that("check_indiv_ids returns list of empty elements for valid metadata", {
  expected_result <- list(
    missing_from_assay = character(0),
    missing_from_clinical = character(0)
  )
  expect_equal(
    check_indiv_ids(valid_clinical, valid_assay),
    expected_result
  )
})

test_that("check_indiv_ids returns list of missing IDs for invalid metadata", {
  expected_result <- list(
    missing_from_assay = character(0),
    missing_from_clinical = "ABC"
  )
  expect_equal(
    check_indiv_ids(invalid_clinical, invalid_assay),
    expected_result
  )
})
