context("test-check-col-names.R")

library("synapser")

test_that("check_col_names returns condition object when check passes", {
  template <- data.frame(x = 1, y = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_true(inherits(result, "check_pass"))
})

test_that("check_col_names returns condition object when check fails", {
  template <- data.frame(x = 1, y = 1, z = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_true(inherits(result, "check_fail"))
})

test_that("check_col_names returns missing columns in the data", {
  template <- data.frame(x = 1, y = 1, z = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  result <- check_col_names(dat, names(template))
  expect_equal(result$data, "z")
})

test_that("get_template fails when not logged in to Synapse", {
  synLogout()
  expect_error(get_template("syn12973252"))
})

if (on_travis()) syn_travis_login() else synLogin()

test_that("check_cols_individual works for individual columns", {
  skip_on_cran()

  cols <- get_template("syn12973254", version = 1)
  full_col <- data.frame(matrix(ncol = length(cols)))
  colnames(full_col) <- cols
  incomplete_col <- full_col[, !names(full_col) %in% "yearsEducation"]

  expect_true(
    inherits(check_cols_individual(full_col, "human"), "check_pass")
  )
  expect_true(
    inherits(check_cols_individual(incomplete_col, "human"), "check_fail")
  )
})

test_that("check_cols_individual returns invalid columns in condition object", {
  skip_on_cran()

  cols <- get_template("syn12973254", version = 1)
  full_col <- data.frame(matrix(ncol = length(cols)))
  colnames(full_col) <- cols
  incomplete_col <- full_col[, !names(full_col) %in% "yearsEducation"]

  expect_equal(
    check_cols_individual(incomplete_col, "human")$data,
    "yearsEducation"
  )
})

test_that("check_cols_biospecimen works for biospecimen columns", {
  skip_on_cran()

  biosp_names <- get_template("syn12973252", version = 4)

  full_col_biosp <- data.frame(matrix(ncol = length(biosp_names)))
  colnames(full_col_biosp) <- biosp_names
  incomplete_col_biosp <- full_col_biosp[, !names(full_col_biosp) == "organ"]

  expect_true(
    inherits(
      check_cols_biospecimen(full_col_biosp, "general"),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_biospecimen(incomplete_col_biosp, "general"),
      "check_fail"
    )
  )
})

test_that("check_cols_biospecimen returns invalid columns in condition obj.", {
  skip_on_cran()

  biosp_names <- get_template("syn12973252", version = 4)

  full_col_biosp <- data.frame(matrix(ncol = length(biosp_names)))
  colnames(full_col_biosp) <- biosp_names
  incomplete_col_biosp <- full_col_biosp[, !names(full_col_biosp) == "organ"]

  expect_equal(
    check_cols_biospecimen(incomplete_col_biosp, "general")$data,
    "organ"
  )
})

test_that("check_cols_biospecimen can get drosophila template", {
  drosophila_names <- get_template("syn20673251", version = 1)
  drosophila_data <- data.frame(matrix(ncol = length(drosophila_names)))
  colnames(drosophila_data) <- drosophila_names

  expect_true(
    inherits(
      check_cols_biospecimen(drosophila_data, "drosophila"),
      "check_pass"
    )
  )
  expect_true(
    inherits(
      check_cols_biospecimen(drosophila_data, "general"),
      "check_fail"
    )
  )
})

test_that("check_cols_assay works for assay columns", {
  skip_on_cran()

  rnaseq_names <- get_template("syn12973256", version = 2)

  full_col_assay <- data.frame(matrix(ncol = length(rnaseq_names)))
  colnames(full_col_assay) <- rnaseq_names
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) == "RIN"]

  expect_true(
    inherits(check_cols_assay(full_col_assay, "rnaSeq"), "check_pass")
  )
  expect_true(
    inherits(check_cols_assay(incomplete_col_assay, "rnaSeq"), "check_fail")
  )
})

test_that("check_cols_assay returns invalid columns within condition object", {
  skip_on_cran()

  rnaseq_names <- get_template("syn12973256", version = 2)

  full_col_assay <- data.frame(matrix(ncol = length(rnaseq_names)))
  colnames(full_col_assay) <- rnaseq_names
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) == "RIN"]

  expect_equal(
    check_cols_assay(incomplete_col_assay, "rnaSeq")$data,
    "RIN"
  )
})

test_that("check_cols_manifest works for manifest columns", {
  cols <- c("path", "parent", "name")
  dat <- data.frame(matrix(ncol = length(cols)))
  names(dat) <- cols
  incomplete <- data.frame(path = "/home/file.txt")

  expect_true(inherits(check_cols_manifest(dat), "check_pass"))
  expect_equal(check_cols_manifest(incomplete)$data, "parent")
})

test_that("get_template errors for files that are not xlsx or csv", {
  expect_error(get_template("syn17039045"))
})

test_that("get_template can read in excel and csv templates", {
  csv <- get_template("syn18384877", version = 1)
  xlsx <- get_template("syn18384878", version = 1)
  expect_equal(csv, c("a", "b", "c"))
  expect_equal(xlsx, c("a", "b", "c"))
})

test_that("get_template can get different version of a template", {
  xlsx1 <- get_template("syn18384878", version = 1)
  xlsx2 <- get_template("syn18384878", version = 2)
  expect_equal(xlsx1, c("a", "b", "c"))
  expect_equal(xlsx2, c("a", "b", "c", "d"))
})

test_that("wrapper functions for specific template gets the correct version", {
  dat <- data.frame(
    individualID = 1,
    specimenID = 1,
    organ = 1,
    tissue = 1,
    BrodmannArea = 1,
    tissueWeight = 1,
    nucleicAcidSource = 1,
    cellType = 1
  )
  expect_true(
    inherits(
      check_cols_biospecimen(dat, "general", version = 2),
      "check_pass"
    )
  )
  expect_equal(
    check_cols_biospecimen(dat, "general", version = 3)$data,
    c("samplingDate", "sampleStatus", "tissueVolume", "fastingState")
  )
})

test_that("check_cols functions handle NULL input", {
  expect_null(check_col_names(NULL))
  expect_null(check_cols_manifest(NULL))
  expect_null(check_cols_individual(NULL))
  expect_null(check_cols_biospecimen(NULL))
  expect_null(check_cols_assay(NULL))
})
