context("test-check-annotation-values.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()
annots <- syndccutils::get_synapse_annotations()

test_that("check_annotation_values returns empty list when no invalid annotations present", {
  dat1 <- data.frame()
  dat2 <- data.frame(assay = "rnaSeq")
  res1 <- check_annotation_values(dat1, annots)
  res2 <- check_annotation_values(dat2, annots)
  expect_equal(res1, structure(list(), .Names = character(0)))
  expect_equal(res2, structure(list(), .Names = character(0)))
})

test_that("check_annotation_values returns invalid annotation values", {
  dat <- data.frame(a = 1, b = 2)
  res <- suppressMessages(check_annotation_values(dat, annots))
  expect_equal(res, structure(list(), .Names = character(0)))
})

test_that("check_annotation_values provides message", {
  dat <- data.frame(assay = "foo", b = 2)
  expect_message(check_annotation_values(dat, annots))
})

test_that("check_annotation_values works for File objects", {
  skip_on_cran()
  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- suppressMessages(check_annotation_values(a, annots))
  resb <- suppressMessages(check_annotation_values(b, annots))
  expect_equal(resa, structure(list(), .Names = character(0)))
  expect_equal(
    resb[order(names(resb))], # need to ensure these are in the right order,
                              # sometimes they get returned in a different order
    list(assay = list("wrongAssay"), species = list("wrongSpecies"))
  )
})

test_that("check_annotation_values works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(check_annotation_values(fv, annots))
  expect_equal(res, list(assay = "wrongAssay", species = "wrongSpecies"))
})

test_that("check annotation values returns unique wrong values, not every single one", {
  dat <- data.frame(assay = c("foo", "foo", "rnaSeq"), stringsAsFactors = FALSE)
  res <- suppressMessages(check_annotation_values(dat, annots))
  expect_equal(res, list(assay = "foo"))
})

test_that("valid_annotation_values returns valid values", {
  dat1 <- data.frame()
  dat2 <- data.frame(assay = "rnaSeq")
  res1 <- valid_annotation_values(dat1)
  res2 <- suppressMessages(valid_annotation_values(dat2, annots))
  ## Returns an empty named list if data frame was empty
  expect_equal(res1, structure(list(), .Names = character(0)))
  ## Returns list of valid values
  expect_equal(
    res2,
    list(assay = structure(1L, .Label = "rnaSeq", class = "factor"))
  )
})

test_that("valid_annotation_values works for File objects", {
  skip_on_cran()
  a <- synGet("syn17038064", downloadFile = FALSE)
  resa <- suppressMessages(valid_annotation_values(a, annots))
  expect_equal(resa, list(fileFormat = list("txt")))
})

test_that("check_annotation_values works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- suppressMessages(valid_annotation_values(fv, annots))
  ## Slightly awkward test because synapse seems to return the values in
  ## different orders sometimes
  expect_equal(names(res), "fileFormat")
  expect_equal(sort(res$fileFormat), c("csv", "txt"))
})

test_that("check_value returns NULL if key is not present", {
  expect_null(check_value("notavalue", "notakey"))
})

test_that("check_value returns valid or invalid valies", {
  a <- check_value("txt", "fileFormat", annots, return_valid = TRUE)
  b <- check_value("wrong", "fileFormat", annots, return_valid = FALSE)
  expect_equal(a, "txt")
  expect_equal(b, "wrong")
})

test_that("check_values checks multiple values", {
  dat <- data.frame(fileFormat = c("wrong", "txt", "csv", "wrong again"))
  resa <- suppressMessages(check_values(dat, annots, return_valid = TRUE))
  resb <- suppressMessages(check_values(dat, annots, return_valid = FALSE))
  expect_equal(
    resa,
    list(
      fileFormat = structure(
        2:1,
        .Label = c("csv", "txt", "wrong",
                   "wrong again"),
        class = "factor"
      )
    )
  )
  expect_equal(
    resb,
    list(
      fileFormat = structure(
        3:4,
        .Label = c("csv", "txt", "wrong",
                   "wrong again"),
        class = "factor"
      )
    )
  )
})


test_that("check_value falls back to get_synapse_annotations", {
  res <- suppressMessages(
    check_value("wrong", "fileFormat", return_valid = FALSE)
  )
  expect_equal(res, "wrong")
  ## Should be the same as passing in annots:
  expect_equal(
    res,
    check_value("wrong", "fileFormat", annots, return_valid = FALSE)
  )
})
