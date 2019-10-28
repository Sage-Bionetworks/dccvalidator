context("test-check-ids-duplicate.R")

test_that("check_indiv_ids_dup fails when column missing", {
  dat <- data.frame(x = 1:5)
  res <- check_indiv_ids_dup(dat)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, c("x"))
})

test_that("check_indiv_ids_dup catches duplicate individual IDs", {
  dat1 <- data.frame(
    individualID = c("foo", "bar", "bar"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(individualID = c(1, 1, 2))
  res1 <- check_indiv_ids_dup(dat1)
  res2 <- check_indiv_ids_dup(dat2)
  expect_true(inherits(res1, "check_fail"))
  expect_equal(res1$data, c("bar"))
  expect_true(inherits(res2, "check_fail"))
  expect_equal(res2$data, c(1))
})

test_that("check_indiv_ids_dup succeeds when all IDs are unique", {
  dat1 <- data.frame(
    individualID = c("foo", "bar", "baz"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(individualID = c(1, 2, 3))
  res1 <- check_indiv_ids_dup(dat1)
  res2 <- check_indiv_ids_dup(dat2)
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_indiv_ids_dup handles NULL input", {
  expect_null(check_indiv_ids_dup(NULL))
})

test_that("check_specimen_ids_dup fails when column missing", {
  dat <- data.frame(x = 1:5)
  res <- check_specimen_ids_dup(dat)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, c("x"))
})

test_that("check_specimen_ids_dup catches duplicate specimen IDs", {
  dat1 <- data.frame(
    specimenID = c("foo", "bar", "bar"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(specimenID = c(1, 1, 2))
  res1 <- check_specimen_ids_dup(dat1)
  res2 <- check_specimen_ids_dup(dat2)
  expect_true(inherits(res1, "check_fail"))
  expect_equal(res1$data, c("bar"))
  expect_true(inherits(res2, "check_fail"))
  expect_equal(res2$data, c(1))
})

test_that("check_specimen_ids_dup succeeds when all IDs are unique", {
  dat1 <- data.frame(
    specimenID = c("foo", "bar", "baz"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(specimenID = c(1, 2, 3))
  res1 <- check_specimen_ids_dup(dat1)
  res2 <- check_specimen_ids_dup(dat2)
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_specimen_ids_dup handles NULL input", {
  expect_null(check_specimen_ids_dup(NULL))
})

test_that("NAs are not treated as duplicates", {
  dat1 <- data.frame(
    specimenID = c(NA, NA, "y", "y"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    individualID = c(NA, NA, "y", "y"),
    stringsAsFactors = FALSE
  )
  res1 <- check_specimen_ids_dup(dat1)
  res2 <- check_specimen_ids_dup(dat1[1:2, , drop = FALSE])
  res3 <- check_indiv_ids_dup(dat2)
  res4 <- check_indiv_ids_dup(dat2[1:2, , drop = FALSE])
  expect_true(inherits(res1, "check_fail"))
  expect_equal(res1$data, "y")
  expect_true(inherits(res2, "check_pass"))
  expect_true(inherits(res3, "check_fail"))
  expect_equal(res3$data, "y")
  expect_true(inherits(res4, "check_pass"))
})

test_that("empty strings are not treated as duplicates", {
  dat1 <- data.frame(specimenID = c("a", "", ""), stringsAsFactors = FALSE)
  dat2 <- data.frame(individualID = c("a", "", ""), stringsAsFactors = FALSE)
  res1 <- check_specimen_ids_dup(dat1)
  res2 <- check_indiv_ids_dup(dat2)
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})
