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

test_that("report_invalid_keys creates a message", {
  expect_message(report_invalid_keys("foo"))
})
