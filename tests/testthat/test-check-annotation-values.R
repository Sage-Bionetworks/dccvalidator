context("test-check-annotation-values.R")

library("tibble")
syn <- attempt_instantiate()
attempt_login(syn)
annots <- tribble(
  ~key, ~value, ~columnType,
  "assay", "rnaSeq", "STRING",
  "fileFormat", "fastq", "STRING",
  "fileFormat", "txt", "STRING",
  "fileFormat", "csv", "STRING",
  "species", "Human", "STRING",
  "organ", "brain", "STRING",
  "BrodmannArea", NA, "STRING",
  "compoundDose", NA, "INTEGER"
)

## check_annotation_values() ---------------------------------------------------

test_that("check_annotation_values returns check_pass when values are valid", {
  dat <- tibble(assay = "rnaSeq")
  res <- check_annotation_values(dat, annots)
  expect_true(inherits(res, "check_pass"))
})

test_that("check_annotation_values errors when data frame is empty", {
  dat <- tibble()
  expect_error(check_annotation_values(dat, annots))
})

test_that("check_annotation_values returns check_fail for invalid annots.", {
  dat <- tibble(assay = "foo", consortium = "bar")
  res <- check_annotation_values(dat, annots)
  expect_true(inherits(res, "check_fail"))
})

test_that("check_annotation_values returns invalid values in $data", {
  dat <- tibble(assay = "foo", species = "bar")
  res <- check_annotation_values(dat, annots)
  expect_equal(res$data, list(assay = "foo", species = "bar"))
})

test_that("check_annotation_values works for File objects", {
  skip_if_not(logged_in(syn = syn))

  a <- syn$get("syn17038064", downloadFile = FALSE)
  b <- syn$get("syn17038065", downloadFile = FALSE)
  resa <- check_annotation_values(a, annots, syn = syn)
  resb <- check_annotation_values(b, annots, syn = syn)
  expect_true(inherits(resa, "check_pass"))
  expect_true(inherits(resb, "check_fail"))
  expect_equal(
    # need to ensure these are in the right order, sometimes they get returned
    # in a different order
    resb$data[order(names(resb$data))],
    list(assay = "wrongAssay", species = "wrongSpecies")
  )
})

test_that("check_annotation_values works for file views", {
  skip_if_not(logged_in(syn = syn))

  fv <- syn$tableQuery("SELECT * FROM syn17038067")
  res <- check_annotation_values(fv, annots)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, list(assay = "wrongAssay", species = "wrongSpecies"))
})

test_that("check annotation values returns *unique* wrong values", {
  dat <- tibble(assay = c("foo", "foo", "rnaSeq"))
  res <- check_annotation_values(dat, annots)
  expect_equal(res$data, list(assay = "foo"))
})

test_that("check_annotation_values handles NULL input", {
  expect_null(check_annotation_values(NULL, annots))
})

## valid_annotation_values() ---------------------------------------------------

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
  skip_if_not(logged_in(syn = syn))

  a <- syn$get("syn17038064", downloadFile = FALSE)
  resa <- valid_annotation_values(a, annots, syn = syn)
  expect_equal(resa, list(fileFormat = "txt"))
})

test_that("valid_annotation_values works for file views", {
  skip_if_not(logged_in(syn = syn))

  fv <- syn$tableQuery("SELECT * FROM syn17038067")
  res <- valid_annotation_values(fv, annots)
  ## Slightly awkward test because synapse seems to return the values in
  ## different orders sometimes
  expect_equal(names(res), "fileFormat")
  expect_equal(sort(res$fileFormat), c("csv", "txt"))
})

test_that("valid_annotation_values handles NULL input", {
  expect_null(valid_annotation_values(NULL, annots))
})

## check_value() ---------------------------------------------------------------

test_that("check_value returns NULL if key is not present", {
  expect_null(check_value("notavalue", "notakey", annots))
})

test_that("check_value returns valid or invalid valies", {
  a <- check_value("txt", "fileFormat", annots, return_valid = TRUE)
  b <- check_value("wrong", "fileFormat", annots, return_valid = FALSE)
  expect_equal(a, "txt")
  expect_equal(b, "wrong")
})

test_that("check_value falls back to get_synapse_annotations", {
  skip_if_not(logged_in(syn = syn))

  res <- check_value("wrong", "fileFormat", return_valid = FALSE, syn = syn)
  expect_equal(res, "wrong")
  ## Should be the same as passing in annots:
  expect_equal(
    res,
    check_value("wrong", "fileFormat", annots, return_valid = FALSE)
  )
})

test_that("check_value checks column type when no enumerated values", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b")
  expect_equal(
    check_value(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
})

test_that("check_value works with a tibble of multiple values", {
  annots <- tribble(
    ~key, ~value, ~columnType,
    "fileFormat", "txt", "STRING",
    "fileFormat", "csv", "STRING",
    "species", "Human", "STRING"
  )
  res <- check_value(values = "txt", key = "fileFormat", annotations = annots)
  expect_equal(res, character(0))
})

## check_values() --------------------------------------------------------------

test_that("check_values checks multiple values", {
  dat <- tibble(fileFormat = c("wrong", "txt", "csv", "wrong again"))
  resa <- check_values(dat, annots, return_valid = TRUE)
  resb <- check_values(dat, annots, return_valid = FALSE)
  expect_equal(
    resa,
    list(fileFormat = c("txt", "csv"))
  )
  expect_equal(
    resb$data,
    list(fileFormat = c("wrong", "wrong again"))
  )
})

test_that("check_values checks that necessary annotation columns are present", {
  annotations <- tibble(key = "x", value = NA)
  a <- tibble(x = c("a", "b"))
  expect_error(check_values(a, annotations))
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
  resd <- check_values(
    dat1,
    annots,
    whitelist_keys = "fileFormat",
    return_valid = TRUE
  )
  rese <- check_values(
    dat1,
    annots,
    whitelist_keys = c("fileFormat", "assay"),
    return_valid = TRUE
  )
  resf <- check_values(
    dat1,
    annots,
    whitelist_keys = c("fileFormat", "tissue"),
    return_valid = TRUE
  )

  expect_equal(names(resa$data), c("assay", "organ"))
  expect_equal(names(resb$data), c("organ"))
  expect_equal(names(resc$data), c("assay", "organ"))
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
  expect_equal(
    resa$data,
    list(fileFormat = "wrongest", assay = "also wrong")
  )
  expect_equal(
    resb$data,
    list(fileFormat = c("wrong", "wronger", "wrongest"))
  )
  expect_equal(
    resc,
    list(fileFormat = "txt", assay = c("rnaSeq", "also wrong"))
  )
})

test_that("check_values can whitelist keys and values simultaneously", {
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
  expect_equal(res$data, list(fileFormat = "wrongest"))
})

test_that("check_values false back to get_synapse_annotations()", {
  skip_if_not(logged_in(syn = syn))

  dat <- tibble(
    fileFormat = c("wrong", "wronger", "wrongest", "txt"),
    assay = c("rnaSeq", "rnaSeq", "rnaSeq", "also wrong")
  )
  res1 <- check_values(dat, annots, syn = syn)
  res2 <- check_values(dat, syn = syn)
  expect_identical(res1, res2)
})

test_that("can customize link in check_values()", {
  dat <- tibble(fileFormat = c("wrong", "txt", "csv", "wrong again"))
  res <- check_values(dat, annots, annots_link = "foo.com")
  expect_true(stringr::str_detect(res$behavior, "foo\\.com"))
})

## check_type() ----------------------------------------------------------------

test_that("check_type returns right value depending on class/`return_valid`", {
  annotations <- tibble(key = "x", columnType = "STRING", value = NA)
  a <- c("a", "b")
  b <- c(1, 2)
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    character(0)
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = FALSE),
    character(0) # values in b are coercible to string
  )
  expect_equal(
    check_type(a, "x", annotations, return_valid = TRUE),
    c("a", "b")
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = TRUE),
    c(1, 2)
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

test_that("check_type can handle annotations as data frames OR tibbles", {
  a1 <- tibble(key = "x", columnType = "STRING", value = NA)
  a2 <- data.frame(
    key = "x",
    columnType = "STRING",
    value = NA,
    stringsAsFactors = FALSE
  )
  a3 <- data.frame(
    key = "x",
    columnType = "STRING",
    value = NA,
    stringsAsFactors = TRUE
  )
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
    check_value(b, "x", annotations, return_valid = FALSE)
  )
})

test_that("check_type omits NAs", {
  annotations <- tibble(key = "x", columnType = "DOUBLE", value = NA)
  a <- c("a", "b", NA)
  b <- c(1, NA, 2)
  expect_equal(
    check_type(a, "x", annotations, return_valid = FALSE),
    c("a", "b")
  )
  expect_equal(
    check_type(b, "x", annotations, return_valid = FALSE),
    character(0)
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

test_that("whitelist_values works in check_type", {
  expect_equal(
    check_type(
      c("a", "b"),
      "compoundDose",
      annots,
      whitelist_values = list(compoundDose = "a")
    ),
    "b"
  )
})

test_that("Multiple column types produces an error", {
  annotations <- tibble(key = c("x", "x"), columnType = c("STRING", "DOUBLE"))
  a <- c("a")
  expect_error(check_type(a, "x", annotations, return_valid = FALSE))
})

## can_coerce() ----------------------------------------------------------------

test_that("anything is coercible to character", {
  expect_true(can_coerce(1, "character"))
  expect_true(can_coerce(1L, "character"))
  expect_true(can_coerce(TRUE, "character"))
  expect_true(can_coerce(as.Date("2020-05-01"), "character"))
  expect_true(can_coerce(as.POSIXct("2020-05-01"), "character"))
  expect_true(can_coerce(as.POSIXlt("2020-05-01"), "character"))
  expect_true(can_coerce(as.complex(-1), "character"))
})

test_that("can_coerce() returns TRUE for integer->numeric", {
  expect_true(can_coerce(1L, "numeric"))
})

test_that("can_coerce() returns FALSE if values aren't coercible", {
  expect_false(can_coerce("a", "numeric"))
  expect_false(can_coerce("a", "logical"))
  expect_false(can_coerce(2.1, "integer"))
})

test_that("can_coerce() returns TRUE for any capitalization of true/false", {
  expect_true(can_coerce("true", "logical"))
  expect_true(can_coerce("True", "logical"))
  expect_true(can_coerce("TRUE", "logical"))
  expect_true(can_coerce(TRUE, "logical"))
  expect_true(can_coerce("false", "logical"))
  expect_true(can_coerce("False", "logical"))
  expect_true(can_coerce("FALSE", "logical"))
  expect_true(can_coerce(FALSE, "logical"))
})

test_that("can_coerce() returns FALSE for values that aren't true/false", {
  expect_false(can_coerce("foo", "logical"))
  expect_false(can_coerce("T", "logical"))
  expect_false(can_coerce("F", "logical"))
  expect_false(can_coerce(c("foo", "True"), "logical"))
  expect_false(can_coerce(c("foo", "True", "FALSE"), "logical"))
  expect_false(can_coerce(1, "logical"))
  expect_false(can_coerce(0, "logical"))
})

test_that("a value can be coerced to its own type", {
  expect_true(can_coerce(1L, "integer"))
  expect_true(can_coerce(1, "numeric"))
  expect_true(can_coerce("foo", "character"))
  expect_true(can_coerce(TRUE, "logical"))
  expect_true(can_coerce(factor("foo"), "factor"))
})

test_that("factors are converted to string before checking coercibility", {
  expect_true(can_coerce(factor("TRUE"), "logical"))
})

test_that("whole numbers that aren't integer type are considered coercible", {
  expect_true(can_coerce(1.0, "integer"))
  expect_true(can_coerce(2.0, "integer"))
  expect_true(can_coerce(c(1.0, 2.0), "integer"))
  expect_false(can_coerce(c(1.0, 2.5), "integer"))
})
