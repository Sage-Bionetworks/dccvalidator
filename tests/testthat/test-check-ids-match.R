context("test-check-ids-match.R")

#######################
####  Create data  ####
#######################

library("tibble")
set.seed(8572)

indIDs <- c("ABC", "DEF", "GHI", "JKL", "MNO") # nolint
specimenIDs <- sprintf("%03d", seq_len(length(indIDs) * 2)) # nolint

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

test_that("check_ids_match returns check_fail if data is missing id column", {
  x <- data.frame(x = 1:10, y = 1:10)
  y <- data.frame(x = 1:5, y = 1:5)
  res1 <- check_ids_match(
    x,
    y,
    idcol = "individualID",
    "individual",
    "biospecimen"
  )
  res2 <- check_ids_match(
    x,
    y,
    idcol = "specimenID",
    "biospecimen",
    "assay"
  )
  expect_true(inherits(res1, "check_fail"))
  expect_true(inherits(res2, "check_fail"))
})

test_that("check_ids_match converts factor columns to character", {
  factor_individual <- valid_individual
  factor_individual$individualID <- as.factor(factor_individual$individualID) # nolint

  factor_biospecimen <- valid_biospecimen
  factor_biospecimen$individualID <- as.factor(factor_biospecimen$individualID) # nolint

  char <- check_ids_match(
    x = valid_individual,
    y = valid_biospecimen,
    idcol = "individualID",
    xname = "individual",
    yname = "biospecimen"
  )
  fact <- check_ids_match(
    x = factor_individual,
    y = factor_biospecimen,
    idcol = "individualID",
    xname = "individual",
    yname = "biospecimen"
  )
  expect_equal(char, fact)
})

test_that("check_ids_match passes when IDs match", {
  res <- check_ids_match(
    x = valid_individual,
    y = valid_biospecimen,
    idcol = "individualID",
    xname = "individual",
    yname = "biospecimen"
  )
  expect_true(inherits(res, "check_pass"))
})

test_that("check_indiv_ids_match catches missing individual IDs", {
  expected_result <- list(
    `Missing from individual` = "ABC",
    `Missing from biospecimen` = character(0)
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
    `Missing from biospecimen` = "010",
    `Missing from assay` = character(0)
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

test_that("Behavior message uses 'x' and 'y' if not provided", {
  res <- check_ids_match(
    invalid_individual,
    invalid_biospecimen,
    idcol = "individualID"
  )
  expect_equal(
    res$behavior,
    "individualID values in the x and y metadata should match."
  )
})

test_that("check_ids_match handles NULL input", {
  dat <- data.frame(individualID = 1:3)
  expect_null(check_ids_match(NULL, dat, "individualID"))
  expect_null(check_ids_match(dat, NULL, "individualID"))
  expect_null(check_ids_match(NULL, NULL, "individualID"))
})

test_that("check_ids_match bidirectional arg looks only in one direction", {
  meta <- data.frame(individualID = 1:4)
  manifest <- data.frame(individualID = 1:2)
  res1 <- check_ids_match(
    x = meta,
    y = manifest,
    idcol = "individualID",
    bidirectional = FALSE
  )
  res2 <- check_ids_match(
    x = manifest,
    y = meta,
    idcol = "individualID",
    bidirectional = FALSE
  )
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_fail"))
  expect_equal(res2$data[[1]], c(3, 4))
})

test_that("check_ids_match bidirectional arg returns relevant data", {
  meta <- data.frame(individualID = 1:4)
  manifest <- data.frame(individualID = c(1:2, 5))
  res1 <- check_ids_match(
    x = meta,
    y = manifest,
    idcol = "individualID",
    bidirectional = TRUE
  )
  res2 <- check_ids_match(
    x = meta,
    y = manifest,
    idcol = "individualID",
    bidirectional = FALSE
  )
  expect_equal(
    res1$data,
    list(`Missing from x` = 5, `Missing from y` = 3:4)
  )
  expect_equal(length(res1$data), 2)
  expect_equal(res2$data, list(`Missing from x` = 5))
  expect_equal(length(res2$data), 1)
})

test_that("check_ids_match data gets default names if not provided", {
  x <- data.frame(individualID = 1:3)
  y <- data.frame(individualID = 4:6)
  res <- check_ids_match(x, y, idcol = "individualID")
  expect_equal(names(res$data), c("Missing from x", "Missing from y"))
})
