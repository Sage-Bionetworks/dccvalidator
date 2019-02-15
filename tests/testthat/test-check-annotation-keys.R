context("test-check-annotation-keys.R")

library("synapser")
library("tibble")
if (on_travis()) syn_travis_login() else synLogin()
annots <- syndccutils::get_synapse_annotations()

test_that("check_annotation_keys returns character(0) when no invalid annotations present", {
  dat <- tibble(assay = "rnaSeq")
  res <- check_annotation_keys(dat, annots)
  expect_equal(res, character(0))
})

test_that("check_annotation_keys errors when no data provided", {
  dat <- tibble()
  expect_error(check_annotation_keys(dat, annots))
})

test_that("check_annotation_keys returns invalid annotation values", {
  dat <- tibble(a = 1, b = 2)
  suppressMessages(res <- check_annotation_keys(dat, annots))
  expect_equal(res, names(dat))
})

test_that("check_annotation_keys provides message", {
  dat <- tibble(a = 1, b = 2)
  expect_message(check_annotation_keys(dat, annots))
})

test_that("check_annotation_keys works for File objects", {
  skip_on_cran()

  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(check_annotation_keys(a, annots))
  resb <- suppressMessages(check_annotation_keys(b, annots))
  expect_equal(resa, character(0))
  expect_equal(resb, "randomAnnotation")
})

test_that("check_annotation_keys works for file views", {
  skip_on_cran()

  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(check_annotation_keys(fv, annots))
  expect_equal(res, "randomAnnotation")
})

test_that("report_keys creates a message", {
  expect_message(report_keys("a message:", "foo"))
})

test_that("valid_annotation_keys returns valid annotation keys", {
  dat1 <- tibble(assay = "rnaSeq")
  dat2 <- tibble(assay = "rnaSeq", fileFormat = "fastq")
  res1 <- suppressMessages(valid_annotation_keys(dat1, annots))
  res2 <- suppressMessages(valid_annotation_keys(dat2, annots))
  expect_equal(res1, "assay")
  expect_equal(res2, c("assay", "fileFormat"))
})

test_that("valid_annotation_keys works for File objects", {
  skip_on_cran()

  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(valid_annotation_keys(a, annots))
  resb <- suppressMessages(valid_annotation_keys(b, annots))
  expect_equal(resa, "fileFormat")
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(resb), c("assay", "fileFormat", "species"))
})

test_that("valid_annotation_keys works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(valid_annotation_keys(fv, annots))
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(res), c("assay", "fileFormat", "species"))
})

test_that("check_keys", {
  test_valid <- "fileFormat"
  test_invalid <- "not a key"
  a1 <- suppressMessages(check_keys(test_valid, annots, return_valid = TRUE))
  a2 <- suppressMessages(check_keys(test_valid, annots, return_valid = FALSE))
  b1 <- suppressMessages(check_keys(test_invalid, annots, return_valid = TRUE))
  b2 <- suppressMessages(check_keys(test_invalid, annots, return_valid = FALSE))
  expect_equal(a1, "fileFormat")
  expect_equal(a2, character(0))
  expect_equal(b1, character(0))
  expect_equal(b2, "not a key")
})

test_that("check_keys falls back to get_synapse_annotations", {
  res <- suppressMessages(check_keys("not a key", return_valid = FALSE))
  expect_equal(res, "not a key")
})

test_that("check_keys checks that necessary annotation columns are present", {
  annotations <- tibble(key = "x", value = NA)
  a <- tibble(x = c("a", "b"))
  expect_error(check_keys(a, annotations))
})
