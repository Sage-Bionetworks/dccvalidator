context("test-check-ids-match.R")

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

test_that("check_ids_match throws error if column is missing", {
  x <- data.frame(x = 1:10, y = 1:10)
  y <- data.frame(x = 1:5, y = 1:5)

  expect_error(check_ids_match(x, y, "foo"))
})

test_that("check_ids_match returns check_fail if data is missing the id column", {
  x <- data.frame(x = 1:10, y = 1:10)
  y <- data.frame(x = 1:5, y = 1:5)
  res1 <- check_ids_match(x, y, idcol = "individualID", "individual", "biospecimen")
  res2 <- check_ids_match(x, y, idcol = "specimenID", "biospecimen", "assay")
  expect_true(inherits(res1, "check_fail"))
  expect_true(inherits(res2, "check_fail"))
})

test_that("check_ids_match converts factor columns to character", {
  factor_individual <- valid_individual
  factor_individual$individualID <- as.factor(factor_individual$individualID)

  factor_biospecimen <- valid_biospecimen
  factor_biospecimen$individualID <- as.factor(factor_biospecimen$individualID)

  char <- check_ids_match(
    valid_individual,
    valid_biospecimen,
    "individualID",
    "individual",
    "biospecimen"
  )
  fact <- check_ids_match(
    factor_individual,
    factor_biospecimen,
    "individualID",
    "individual",
    "biospecimen"
  )
  expect_equal(char, fact)
})

test_that("check_ids_match passes when IDs match", {
  res <- check_ids_match(
    valid_individual,
    valid_biospecimen,
    "individualID",
    "individual",
    "biospecimen"
  )
  expect_true(inherits(res, "check_pass"))
})

test_that("check_indiv_ids_match catches missing individual IDs", {
  expected_result <- list(
    missing_from_x = "ABC",
    missing_from_y = character(0)
  )
  res <- check_indiv_ids_match(
    invalid_individual,
    invalid_biospecimen,
    "individual",
    "biospecimen"
  )
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, expected_result)
})

test_that("check_specimen_ids_match catches missing specimen IDs", {
  expected_result <- list(
    missing_from_x = "010",
    missing_from_y = character(0)
  )
  res <- check_specimen_ids_match(
    invalid_biospecimen,
    valid_assay,
    "biospecimen",
    "assay"
  )
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, expected_result)
})

test_that("Behavior message leaves out xname/yname if not provided", {
  res <- check_ids_match(
    invalid_individual,
    invalid_biospecimen,
    idcol = "individualID"
  )
  expect_equal(res$behavior, "individualID values should match.")
})
