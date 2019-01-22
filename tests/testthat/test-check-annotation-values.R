context("test-check-annotation-values.R")

test_that("check_annotation_values returns empty list when no invalid annotations present", {
  dat1 <- data.frame()
  dat2 <- data.frame(assay = "rnaSeq")
  res1 <- check_annotation_values(dat1)
  res2 <- check_annotation_values(dat2)
  ## Returns an empty list if data frame was empty
  expect_equal(res1, list())
  ## Returns an empty named list if nothing was invalid
  expect_equal(res2, structure(list(), .Names = character(0)))
})

test_that("check_annotation_values returns invalid annotation values", {
  dat <- data.frame(a = 1, b = 2)
  res <- suppressMessages(check_annotation_values(dat))
  expect_equal(res, structure(list(), .Names = character(0)))
})

test_that("check_annotation_values provides message", {
  dat <- data.frame(assay = "foo", b = 2)
  expect_message(check_annotation_values(dat))
})

test_that("check_annotation_values works for File objects", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(check_annotation_values(a))
  resb <- suppressMessages(check_annotation_values(b))
  expect_equal(resa, structure(list(), .Names = character(0)))
  expect_equal(
    resb[order(names(resb))], # need to ensure these are in the right order,
                              # sometimes they get returned in a different order
    list(assay = list("wrongAssay"), species = list("wrongSpecies"))
  )
})

test_that("check_annotation_values works for file views", {
  skip_on_cran()

  library("synapser")
  if (on_travis()) {
    syn_travis_login()
  } else {
    synLogin()
  }
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(check_annotation_values(fv))
  expect_equal(res, list(assay = "wrongAssay", species = "wrongSpecies"))
})

test_that("check annotation values returns unique wrong values, not every single one", {
  dat <- data.frame(assay = c("foo", "foo", "rnaSeq"), stringsAsFactors = FALSE)
  res <- suppressMessages(check_annotation_values(dat))
  expect_equal(res, list(assay = "foo"))
})
