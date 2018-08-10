context("test-check-col-names.R")

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


test_that("check_cols_individual works for individual columns", {
  cols <- individual_cols()[["human"]]
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

test_that("check_cols_assay works for assay columns", {

  full_col_assay <- data.frame(matrix(ncol = length(assay_cols()[["rnaSeq"]])))
  colnames(full_col_assay) <- assay_cols()[["rnaSeq"]]
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) %in% c("tissue", "RIN")]

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
  cols <- c("path", "parent", "name", "used", "executed")
  dat <- data.frame(matrix(ncol = length(cols)))
  names(dat) <- cols

  expect_equal(check_cols_manifest(dat), character(0))
  expect_equal(check_cols_manifest(dat[, c("path", "parent")]), c("name", "used", "executed"))

})
