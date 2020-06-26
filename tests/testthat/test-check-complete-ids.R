context("test-check-existing-ids.R")

samples_table <- tibble::tribble(
  ~individualID, ~specimenID,   ~study,   ~assay,
            "A",        "a1", "study1", "rnaSeq",
            "A",        "a2", "study1", "rnaSeq",
            "B",        "b1", "study1", "rnaSeq",
            "B",        "b2", "study1", "rnaSeq",
            "B",        "b1", "study1", "rnaSeq",
            "C",        "c1", "study1",  "LC-MS",
            "D",        "d1", "study2", "rnaSeq",
            "D",        "d2", "study2", "rnaSeq"
  )

test_that("check_complete_ids returns NULL if no data provided", {
  expect_null(check_complete_ids(NULL))
})

test_that("check_complete_ids errors if study is not present in master table", {
  dat <- data.frame(individualID = c("x", "y", "z"))
  expect_error(
    check_complete_ids(
      dat,
      samples_table,
      study = "foo",
      id_type = "individualID"
    )
  )
})

test_that("check_complete_ids fails if ID column is not present in data", {
  dat <- data.frame(individualID = c("x", "y", "z"))
  res <- check_complete_ids(
    dat,
    samples_table,
    study = "study1",
    id_type = "specimenID"
  )
  expect_true(inherits(res, "check_fail"))
})

test_that("check_complete_ids looks at assay of interest", {
  dat1 <- data.frame(specimenID = c("a1", "a2", "b1", "b2"))
  dat2 <- data.frame(specimenID = "c1")
  res1 <- check_complete_ids(
    dat1,
    samples_table,
    study = "study1",
    id_type = "specimenID",
    assay = "rnaSeq"
  )
  res2 <- check_complete_ids(
    dat2,
    samples_table,
    study = "study1",
    id_type = "specimenID",
    assay = "LC-MS"
  )
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_complete_ids looks at IDs in study of interest", {
  dat1 <- data.frame(individualID = c("A", "B", "C"))
  dat2 <- data.frame(individualID = "D")
  res1 <- check_complete_ids(
    dat1,
    samples_table,
    study = "study1",
    id_type = "individualID"
  )
  res2 <- check_complete_ids(
    dat2,
    samples_table,
    study = "study2",
    id_type = "individualID"
  )
  res3 <- check_complete_ids(
    dat1,
    samples_table,
    study = "study2",
    id_type = "individualID"
  )
  res4 <- check_complete_ids(
    dat2,
    samples_table,
    study = "study1",
    id_type = "individualID"
  )
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
  expect_true(inherits(res3, "check_fail"))
  expect_true(inherits(res4, "check_fail"))
})

test_that("missing IDs are included in data", {
  dat <- data.frame(individualID = c("A", "B"))
  res <- check_complete_ids(
    dat,
    samples_table,
    study = "study1",
    id_type = "individualID"
  )
  expect_equal(res$data, "C")
})
