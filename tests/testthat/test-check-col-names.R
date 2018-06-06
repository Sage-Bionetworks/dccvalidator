context("test-check-col-names.R")

test_that("check_col_names returns empty character vector when all columns present", {
  full_col_indiv <- data.frame(matrix(ncol = length(templist()$individual)))
  colnames(full_col_indiv) <- templist()$individual

  full_col_assay <- data.frame(matrix(ncol = length(templist()$assay_rnaseq)))
  colnames(full_col_assay) <- templist()$assay_rnaseq

  expect_equal(check_col_names(full_col_indiv, "individual"), character(0))
  expect_equal(check_col_names(full_col_assay, "assay_rnaseq"), character(0))
})

test_that("check_col_names returns vector of missing columns", {
  full_col_indiv <- data.frame(matrix(ncol = length(templist()$individual)))
  colnames(full_col_indiv) <- templist()$individual
  incomplete_col_indiv <- full_col_indiv[, !names(full_col_indiv) %in% "years_of_education"]

  full_col_assay <- data.frame(matrix(ncol = length(templist()$assay_rnaseq)))
  colnames(full_col_assay) <- templist()$assay_rnaseq
  incomplete_col_assay <- full_col_assay[, !names(full_col_assay) %in% c("tissue", "RIN")]

  expect_equal(check_col_names(incomplete_col_indiv, "individual"), "years_of_education")
  expect_equal(check_col_names(incomplete_col_assay, "assay_rnaseq"), c("tissue", "RIN"))
})
