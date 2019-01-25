context("test-check-annotation-keys.R")

test_that("check_annotation_keys returns character(0) when no invalid annotations present", {
  dat1 <- data.frame()
  dat2 <- data.frame(assay = "rnaSeq")
  res1 <- check_annotation_keys(dat1)
  res2 <- check_annotation_keys(dat2)
  expect_equal(res1, character(0))
  expect_equal(res2, character(0))
})

test_that("check_annotation_keys returns invalid annotation values", {
  dat <- data.frame(a = 1, b = 2)
  suppressMessages(res <- check_annotation_keys(dat))
  expect_equal(res, names(dat))
})

test_that("check_annotation_keys provides message", {
  dat <- data.frame(a = 1, b = 2)
  expect_message(check_annotation_keys(dat))
})

test_that("check_annotation_keys works for File objects", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(check_annotation_keys(a))
  resb <- suppressMessages(check_annotation_keys(b))
  expect_equal(resa, character(0))
  expect_equal(resb, "randomAnnotation")
})

test_that("check_annotation_keys works for file views", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(check_annotation_keys(fv))
  expect_equal(res, "randomAnnotation")
})

test_that("report_keys creates a message", {
  expect_message(report_keys("a message:", "foo"))
})

test_that("valid_annotation_keys returns valid annotation keys", {
  dat1 <- data.frame(assay = "rnaSeq")
  dat2 <- data.frame(assay = "rnaSeq", fileFormat = "fastq")
  res1 <- suppressMessages(valid_annotation_keys(dat1))
  res2 <- suppressMessages(valid_annotation_keys(dat2))
  expect_equal(res1, "assay")
  expect_equal(res2, c("assay", "fileFormat"))
})

test_that("valid_annotation_keys works for File objects", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(valid_annotation_keys(a))
  resb <- suppressMessages(valid_annotation_keys(b))
  expect_equal(resa, "fileFormat")
  expect_equal(resb, c("species", "assay", "fileFormat"))
})

test_that("valid_annotation_keys works for file views", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(valid_annotation_keys(fv))
  expect_equal(res, c("fileFormat", "assay", "species"))
})

test_that("check_keys", {
  test_valid <- "fileFormat"
  test_invalid <- "not a key"
  a1 <- suppressMessages(check_keys(test_valid, return_valid = TRUE))
  a2 <- suppressMessages(check_keys(test_valid, return_valid = FALSE))
  b1 <- suppressMessages(check_keys(test_invalid, return_valid = TRUE))
  b2 <- suppressMessages(check_keys(test_invalid, return_valid = FALSE))
  expect_equal(a1, "fileFormat")
  expect_equal(a2, character(0))
  expect_equal(b1, character(0))
  expect_equal(b2, "not a key")
})
