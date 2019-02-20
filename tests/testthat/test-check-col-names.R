context("test-check-col-names.R")

library("synapser")

test_that("check_col_names returns empty character vector when all columns present", {
  template <- data.frame(x = 1, y = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  expect_equal(check_col_names(dat, names(template)), character(0))
})

test_that("check_col_names returns vector of missing columns", {
  template <- data.frame(x = 1, y = 1, z = 1)
  dat <- data.frame(x = 5:10, y = 5:10)
  expect_equal(check_col_names(dat, names(template)), "z")
})


test_that("get_template fails when not logged in to Synapse", {
  synLogout()
  expect_error(get_template("biospecimen"))
})

if (on_travis()) syn_travis_login() else synLogin()

test_that("check_cols_individual works for individual columns", {
  skip_on_cran()

  cols <- get_template("human")
  full_col_indiv <- data.frame(matrix(ncol = length(cols)))
  colnames(full_col_indiv) <- cols
  incomplete_col_indiv <- full_col_indiv[, !names(full_col_indiv) %in% "yearsEducation"]

  expect_equal(
    check_cols_individual(full_col_indiv, "human"),
    character(0)
  )
  expect_equal(
    check_cols_individual(incomplete_col_indiv, "human"),
    "yearsEducation"
  )
})

test_that("check_cols_biospecimen works for biospecimen columns", {
  skip_on_cran()

  biosp_names <- get_template("biospecimen")

  full_col_biosp <- data.frame(matrix(ncol = length(biosp_names)))
  colnames(full_col_biosp) <- biosp_names
  incomplete_col_biosp <- full_col_biosp[, !names(full_col_biosp) == "organ"]

  expect_equal(
    check_cols_biospecimen(full_col_biosp),
    character(0)
  )
  expect_equal(
    check_cols_biospecimen(incomplete_col_biosp),
    "organ"
  )
})

test_that("check_cols_assay works for assay columns", {
  skip_on_cran()

  rnaseq_names <- get_template("rnaSeq")

  full_col_assay <- data.frame(matrix(ncol = length(rnaseq_names)))
  colnames(full_col_assay) <- rnaseq_names
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) == "RIN"]

  expect_equal(
    check_cols_assay(full_col_assay, "rnaSeq"),
    character(0)
  )
  expect_equal(
    check_cols_assay(incomplete_col_assay, "rnaSeq"),
    "RIN"
  )
})

test_that("check_cols_manifest works for manifest columns", {
  cols <- c("path", "parent", "name")
  dat <- data.frame(matrix(ncol = length(cols)))
  names(dat) <- cols
  incomplete <- data.frame(path = "/home/file.txt")

  expect_equal(check_cols_manifest(dat), character(0))
  expect_equal(check_cols_manifest(incomplete), "parent")
})
