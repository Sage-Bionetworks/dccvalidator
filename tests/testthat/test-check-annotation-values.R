context("test-check-annotation-values.R")

library("synapser")
library("tibble")
if (on_travis()) syn_travis_login() else synLogin()
annots <- syndccutils::get_synapse_annotations()

test_that("check_annotation_values returns empty list when no invalid annotations present", {
  dat <- tibble(assay = "rnaSeq")
  res <- check_annotation_values(dat, annots)
  expect_equal(res, structure(list(), .Names = character(0)))
})

test_that("check_annotation_values errors when there are no annotations to check", {
  dat <- tibble()
  expect_error(check_annotation_values(dat, annots))
})

test_that("check_annotation_values returns invalid annotation values", {
  dat <- tibble(assay = "foo", consortium = "bar")
  res <- check_annotation_values(dat, annots)
  expect_equal(res, list(assay = "foo", consortium = "bar"))
})

test_that("check_annotation_values works for File objects", {
  skip_on_cran()
  a <- synGet("syn17038064", downloadFile = FALSE)
  b <- synGet("syn17038065", downloadFile = FALSE)
  resa <- check_annotation_values(a, annots)
  resb <- check_annotation_values(b, annots)
  expect_equal(resa, structure(list(), .Names = character(0)))
  expect_equal(
    resb[order(names(resb))], # need to ensure these are in the right order,
                              # sometimes they get returned in a different order
    list(assay = "wrongAssay", species = "wrongSpecies")
  )
})

test_that("check_annotation_values works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- check_annotation_values(fv, annots)
  expect_equal(res, list(assay = "wrongAssay", species = "wrongSpecies"))
})

test_that("check annotation values returns unique wrong values, not every single one", {
  dat <- tibble(assay = c("foo", "foo", "rnaSeq"))
  res <- check_annotation_values(dat, annots)
  expect_equal(res, list(assay = "foo"))
})

test_that("valid_annotation_values returns valid values", {
  dat <- tibble(assay = "rnaSeq")
  res <- valid_annotation_values(dat, annots)
  ## Returns list of valid values
  expect_equal(res, list(assay = "rnaSeq"))
})

test_that("valid_annotation_values fails when no annotations present", {
  dat <- tibble()
  expect_error(valid_annotation_values(dat, annots))
})

test_that("valid_annotation_values works for File objects", {
  skip_on_cran()
  a <- synGet("syn17038064", downloadFile = FALSE)
  resa <- valid_annotation_values(a, annots)
  expect_equal(resa, list(fileFormat = "txt"))
})

test_that("check_annotation_values works for file views", {
  skip_on_cran()
  fv <- synTableQuery("SELECT * FROM syn17038067")
  res <- valid_annotation_values(fv, annots)
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
  dat <- tibble(fileFormat = c("wrong", "txt", "csv", "wrong again"))
  resa <- check_values(dat, annots, return_valid = TRUE)
  resb <- check_values(dat, annots, return_valid = FALSE)
  expect_equal(
    resa,
    list(fileFormat = c("txt", "csv"))
  )
  expect_equal(
    resb,
    list(fileFormat = c("wrong", "wrong again"))
  )
})

test_that("check_value falls back to get_synapse_annotations", {
  res <- check_value("wrong", "fileFormat", return_valid = FALSE)
  expect_equal(res, "wrong")
  ## Should be the same as passing in annots:
  expect_equal(
    res,
    check_value("wrong", "fileFormat", annots, return_valid = FALSE)
  )
})

test_that("check_type returns right value depending on class and `return_valid` option", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b")
  b <- c(1, 2)
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = FALSE),
    c(1, 2)
  )
  expect_equal(
    check_type(a, "x", annotations, return_valid = TRUE),
    c("a", "b")
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = TRUE),
    character(0)
  )
})

test_that("check_type checks different classes", {
  annotations <- tibble(key = "x", columnType = "BOOLEAN", value = NA)
  a <- c("a", "b")
  b <- c(1, 2)
  c <- c(TRUE, FALSE)
  d <- c(1L, 2L)
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    c("a", "b")
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = FALSE),
    c(1, 2)
  )
  expect_equal(
    check_type(c, "x", annotations, return_valid = FALSE),
    character(0)
  )
  expect_equal(
    check_type(d, "x", annotations, return_valid = FALSE),
    c(1L, 2L)
  )
})

test_that("check_value checks column type when enumerated values aren't defined", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b")
  expect_equal(
    check_value(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
})

test_that("check_type can handle annotations as either data frames or tibbles", {
  a1 <- tibble(key = "x", columnType = "STRING", value = NA)
  a2 <- data.frame(key = "x", columnType = "STRING", value = NA, stringsAsFactors = FALSE)
  a3 <- data.frame(key = "x", columnType = "STRING", value = NA, stringsAsFactors = TRUE)
  a <- c("a", "b")
  expect_equal(
    check_value(a, "x", a1, return_valid = FALSE),
    check_value(a, "x", a2, return_valid = FALSE)
  )
  expect_equal(
    check_value(a, "x", a2, return_valid = FALSE),
    check_value(a, "x", a3, return_valid = FALSE)
  )
})

test_that("check_type can handle factor annotation values as strings", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b")
  b <- factor(c("a", "b"))
  expect_equal(
    check_value(a, "x", annotations, return_valid = FALSE),
    check_value(b, "x", annotations, return_valid = FALSE),
  )
})

test_that("check_values checks that necessary annotation columns are present", {
  annotations <- tibble(key = "x", value = NA)
  a <- tibble(x = c("a", "b"))
  expect_error(check_values(a, annotations))
})

test_that("check_type omits NAs", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b", NA)
  b <- c(1, NA, 2)
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = FALSE),
    c(1, 2)
  )
})

test_that("check_type does not return duplicates", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b", NA, "b", "a")
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
})

test_that("check_values can whitelist certain keys", {
  dat1 <- tibble(
    fileFormat = "wrong",
    assay = "also wrong",
    organ = "wrong again"
  )
  resa <- check_values(dat1, annots, whitelist_keys = "fileFormat")
  resb <- check_values(dat1, annots, whitelist_keys = c("fileFormat", "assay"))
  resc <- check_values(dat1, annots, whitelist_keys = c("fileFormat", "tissue"))

  dat2 <- tibble(
    fileFormat = "txt",
    assay = "rnaSeq",
    organ = "brain"
  )
  resd <- check_values(dat1, annots, whitelist_keys = "fileFormat", return_valid = TRUE)
  rese <- check_values(dat1, annots, whitelist_keys = c("fileFormat", "assay"), return_valid = TRUE)
  resf <- check_values(dat1, annots, whitelist_keys = c("fileFormat", "tissue"), return_valid = TRUE)

  expect_equal(names(resa), c("assay", "organ"))
  expect_equal(names(resb), c("organ"))
  expect_equal(names(resc), c("assay", "organ"))
  expect_equal(names(resd), c("fileFormat"))
  expect_equal(names(rese), c("fileFormat", "assay"))
  expect_equal(names(resf), c("fileFormat"))
})

test_that("check_values can whitelist certain key/value combinations", {
  dat <- tibble(
    fileFormat = c("wrong", "wronger", "wrongest", "txt"),
    assay = c("rnaSeq", "rnaSeq", "rnaSeq", "also wrong")
  )
  resa <- check_values(
    dat,
    annots,
    whitelist_values = list(fileFormat = c("wrong", "wronger"))
  )
  resb <- check_values(
    dat,
    annots,
    whitelist_values = list(assay = "also wrong")
  )
  resc <- check_values(
    dat,
    annots,
    whitelist_values = list(assay = "also wrong"),
    return_valid = TRUE
  )
  expect_equal(resa, list(fileFormat = "wrongest", assay = "also wrong"))
  expect_equal(resb, list(fileFormat = c("wrong", "wronger", "wrongest")))
  expect_equal(resc, list(fileFormat = "txt", assay = c("rnaSeq", "also wrong")))
})

test_that("can whitelist keys and values simultaneously", {
  dat <- tibble(
    fileFormat = c("wrong", "wronger", "wrongest", "txt"),
    assay = c("rnaSeq", "rnaSeq", "rnaSeq", "also wrong")
  )
  res <- check_values(
    dat,
    annots,
    whitelist_keys = "assay",
    whitelist_values = list(fileFormat = c("wrong", "wronger"))
  )
  expect_equal(res, list(fileFormat = "wrongest"))
})

test_that("whitelist_values works in check_type", {
  expect_equal(
    check_type(
      1:3,
      "BrodmannArea",
      annots,
      whitelist_values = list(BrodmannArea = 1:2)
    ),
    3
  )
})
