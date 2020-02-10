context("test-check-annotation-keys.R")

library("tibble")
syn <- attempt_instantiate()
attempt_login(syn = syn)

annots <- tribble(
  ~key, ~value, ~columnType,
  "assay", "rnaSeq", "STRING",
  "fileFormat", "fastq", "STRING",
  "fileFormat", "txt", "STRING",
  "fileFormat", "csv", "STRING",
  "species", "Human", "STRING"
)

test_that("check_annotation_keys returns check_pass when all are valid", {
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
  skip_if_not(logged_in(syn = syn))

  a <- syn$get("syn17038064", downloadFile = FALSE)
  b <- syn$get("syn17038065", downloadFile = FALSE)
  resa <- check_annotation_keys(a, annots, syn)
  resb <- check_annotation_keys(b, annots, syn)

  expect_true(inherits(resa, "check_pass"))
  expect_true(inherits(resb, "check_fail"))
  expect_equal(resa$data, NULL)
  expect_equal(resb$data, "randomAnnotation")
})

test_that("check_annotation_keys works for file views", {
  skip_if_not(logged_in(syn = syn))

  fv <- syn$tableQuery("SELECT * FROM syn17038067")
  res <- check_annotation_keys(fv, annots)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, "randomAnnotation")
})

test_that("check_annotation_keys handles NULL input", {
  expect_null(check_annotation_keys(NULL, annots))
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
  skip_if_not(logged_in(syn = syn))

  a <- syn$get("syn17038064", downloadFile = FALSE)
  b <- syn$get("syn17038065", downloadFile = FALSE)
  resa <- valid_annotation_keys(a, annots, syn)
  resb <- valid_annotation_keys(b, annots, syn)
  expect_equal(resa, "fileFormat")
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(resb), c("assay", "fileFormat", "species"))
})

test_that("valid_annotation_keys works for file views", {
  skip_if_not(logged_in(syn = syn))

  fv <- syn$tableQuery("SELECT * FROM syn17038067")
  res <- valid_annotation_keys(fv, annots)
  ## Sort because I think Synapse doesn't always return the same order
  expect_equal(sort(res), c("assay", "fileFormat", "species"))
})

test_that("valid_annotation_keys handles NULL input", {
  expect_null(valid_annotation_keys(NULL, annots))
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
  skip_if_not(logged_in(syn = syn))

  res <- check_keys("not a key", return_valid = FALSE, syn = syn)
  expect_equal(res$data, "not a key")
})

test_that("check_keys checks necessary annotation columns are present", {
  annotations <- tibble(key = "x", value = NA)
  a <- tibble(x = c("a", "b"))
  expect_error(check_keys(a, annotations))
})

test_that("check_keys allows whitelisting keys", {
  resa <- check_keys(
    c("assay", "foo"),
    annotations = annots,
    whitelist_keys = "foo"
  )
  resb <- check_keys(
    c("assay", "foo", "bar"),
    annotations = annots,
    whitelist_keys = "foo"
  )
  resc <- check_keys(
    c("assay", "foo"),
    annotations = annots,
    whitelist_keys = c("foo", "bar")
  )
  resd <- check_keys(
    c("assay", "foo"),
    annotations = annots,
    whitelist_keys = "foo",
    return_valid = TRUE
  )
  rese <- check_keys(
    c("assay", "foo", "bar"),
    annotations = annots,
    whitelist_keys = "foo",
    return_valid = TRUE
  )
  resf <- check_keys(
    c("assay", "foo"),
    annotations = annots,
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

test_that("can customize link in check_keys()", {
  res <- check_keys("foo", annots, annots_link = "foo.com")
  expect_true(stringr::str_detect(res$behavior, "foo\\.com"))
})
