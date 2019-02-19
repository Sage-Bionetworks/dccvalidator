context("test-check-ids.R")

#######################
####  Create data  ####
#######################

library("tibble")
set.seed(8572)

indIDs    <- c("ABC", "DEF", "GHI", "JKL", "MNO")
specimenIDs <- sprintf("%03d", seq_len(length(indIDs) * 2))

## Valid data

valid_individual <- tibble(
  individualID = indIDs,
  age = sample(18:50, size = 5)
)

valid_biospecimen <- tibble(
  individualID = rep(indIDs, 2),
  specimenID = specimenIDs
)

valid_assay <- tibble(
  specimenID = specimenIDs,
  filename = paste0(LETTERS[seq_along(specimenIDs)], ".bam")
)

## Invalid data

## Missing one row, i.e. one individual ID
invalid_individual <- valid_individual[-1, ]

## Missing one row, i.e. one sample ID (all individual IDs are present because
## they repeat 2x and only one row is missing)
invalid_biospecimen <- valid_biospecimen[-10, ]

test_that("check_ids throws error if column is missing", {
  x <- data.frame(x = 1:10, y = 1:10)
  y <- data.frame(x = 1:5, y = 1:5)

  expect_error(check_ids(x, y, "foo"))
})

test_that("check_ids converts factor columns to character", {
  factor_individual <- valid_individual
  factor_individual$individualID <- as.factor(factor_individual$individualID)

  factor_biospecimen <- valid_biospecimen
  factor_biospecimen$individualID <- as.factor(factor_biospecimen$individualID)

  char <- check_ids(valid_individual, valid_biospecimen, "individualID")
  fact <- check_ids(factor_individual, factor_biospecimen, "individualID")
  expect_equal(char, fact)
})

test_that("check_indiv_ids returns list of empty elements for valid metadata", {
  expected_result <- list(
    missing_from_x = character(0),
    missing_from_y = character(0)
  )
  expect_equal(
    check_indiv_ids(valid_individual, valid_biospecimen),
    expected_result
  )
})

test_that("check_indiv_ids returns list of missing individual IDs for invalid metadata", {
  expected_result <- list(
    missing_from_x = "ABC",
    missing_from_y = character(0)
  )
  expect_equal(
    check_indiv_ids(invalid_individual, invalid_biospecimen),
    expected_result
  )
})

test_that("check_specimen_ids returns list of empty elements for valid metadata", {
  expected_result <- list(
    missing_from_x = character(0),
    missing_from_y = character(0)
  )
  expect_equal(
    check_specimen_ids(valid_biospecimen, valid_assay),
    expected_result
  )
})

test_that("check_specimen_ids returns list of missing specimen IDs for invalid metadata", {
  expected_result <- list(
    missing_from_x = "010",
    missing_from_y = character(0)
  )
  expect_equal(
    check_specimen_ids(invalid_biospecimen, valid_assay),
    expected_result
  )
})
