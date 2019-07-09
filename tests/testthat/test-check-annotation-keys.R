context("test-check-annotation-keys.R")

library("synapser")
library("tibble")
if (on_travis()) syn_travis_login() else synLogin()
annots <- syndccutils::get_synapse_annotations()

test_that("check_annotation_keys returns check_pass when no invalid annotations present", {
  dat <- tibble(assay = "rnaSeq")
  res <- check_annotation_keys(dat, annots)
  expect_true(inherits(res, "check_pass"))
})

test_that("check_annotation_keys errors when no data provided", {
  dat <- tibble()
  expect_error(check_annotation_keys(dat, annots))
})

test_that("check_annotation_keys returns invalid annotation values", {
  dat <- tibble(a = 1, b = 2)
  res <- check_annotation_keys(dat, annots)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, names(dat))
})

test_that("check_annotation_keys works for File objects", {
  skip_on_cran()

  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- check_annotation_keys(a, annots)
  resb <- check_annotation_keys(b, annots)

  expect_true(inherits(resa, "check_pass"))
  expect_true(inherits(resb, "check_fail"))
  expect_equal(resa$data, NULL)
  expect_equal(resb$data, "randomAnnotation")
})

test_that("check_annotation_keys works for file views", {
  skip_on_cran()

  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- check_annotation_keys(fv, annots)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, "randomAnnotation")
})

test_that("valid_annotation_keys returns valid annotation keys", {
  dat1 <- tibble(assay = "rnaSeq")
  dat2 <- tibble(assay = "rnaSeq", fileFormat = "fastq")
  res1 <- valid_annotation_keys(dat1, annots)
  res2 <- valid_annotation_keys(dat2, annots)
  expect_equal(res1, "assay")
  expect_equal(res2, c("assay", "fileFormat"))
})

test_that("valid_annotation_keys works for File objects", {
  skip_on_cran()

  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- valid_annotation_keys(a, annots)
  resb <- valid_annotation_keys(b, annots)
  expect_equal(resa, "fileFormat")
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(resb), c("assay", "fileFormat", "species"))
})

test_that("valid_annotation_keys works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- valid_annotation_keys(fv, annots)
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(res), c("assay", "fileFormat", "species"))
})

test_that("check_keys", {
  test_valid <- "fileFormat"
  test_invalid <- "not a key"
  a1 <- check_keys(test_valid, annots, return_valid = TRUE)
  a2 <- check_keys(test_valid, annots, return_valid = FALSE)
  b1 <- check_keys(test_invalid, annots, return_valid = TRUE)
  b2 <- check_keys(test_invalid, annots, return_valid = FALSE)


  expect_equal(a1, "fileFormat")
  expect_equal(a2$data, NULL)
  expect_true(inherits(a2, "check_pass"))
  expect_equal(b1, character(0))
  expect_equal(b2$data, "not a key")
  expect_true(inherits(b2, "check_fail"))
})

test_that("check_keys falls back to get_synapse_annotations", {
  res <- check_keys("not a key", return_valid = FALSE)
  expect_equal(res$data, "not a key")
})

test_that("check_keys checks that necessary annotation columns are present", {
  annotations <- tibble(key = "x", value = NA)
  a <- tibble(x = c("a", "b"))
  expect_error(check_keys(a, annotations))
})

test_that("check_keys allows whitelisting keys that aren't part of the annotations", {
  resa <- check_keys(c("assay", "foo"), whitelist_keys = "foo")
  resb <- check_keys(c("assay", "foo", "bar"), whitelist_keys = "foo")
  resc <- check_keys(c("assay", "foo"), whitelist_keys = c("foo", "bar"))
  resd <- check_keys(
    c("assay", "foo"),
    whitelist_keys = "foo",
    return_valid = TRUE
  )
  rese <- check_keys(
    c("assay", "foo", "bar"),
    whitelist_keys = "foo",
    return_valid = TRUE
  )
  resf <- check_keys(
    c("assay", "foo"),
    whitelist_keys = c("foo", "bar"),
    return_valid = TRUE
  )
  expect_true(inherits(resa, "check_pass"))
  expect_true(inherits(resb, "check_fail"))
  expect_equal(resb$data, "bar")
  expect_true(inherits(resc, "check_pass"))
  expect_equal(resd, c("assay", "foo"))
  expect_equal(rese, c("assay", "foo"))
  expect_equal(resf, c("assay", "foo"))
})
