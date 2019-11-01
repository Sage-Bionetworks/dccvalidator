context("test-is-study-name-valid.R")

test_that("is_study_name_valid returns TRUE when name is valid", {
  name1 <- "study"
  name2 <- "study name"
  name3 <- "study_name"
  name4 <- "study (name)"
  name5 <- "study.name"
  name6 <- "study-name"
  name7 <- "study1"
  name8 <- "study+"
  expect_true(is_study_name_valid(name1))
  expect_true(is_study_name_valid(name2))
  expect_true(is_study_name_valid(name3))
  expect_true(is_study_name_valid(name4))
  expect_true(is_study_name_valid(name5))
  expect_true(is_study_name_valid(name6))
  expect_true(is_study_name_valid(name7))
  expect_true(is_study_name_valid(name8))
})

test_that("is_study_name_valid returns FALSE when name is invalid", {
  name1 <- "study#"
  name2 <- "study$"
  name3 <- "study!"
  name4 <- "study>"
  name5 <- "study[]"
  name6 <- "study{}"
  name7 <- "study@"
  name8 <- "study&"
  name9 <- "study*"
  name10 <- ""
  expect_false(is_study_name_valid(name1))
  expect_false(is_study_name_valid(name2))
  expect_false(is_study_name_valid(name3))
  expect_false(is_study_name_valid(name4))
  expect_false(is_study_name_valid(name5))
  expect_false(is_study_name_valid(name6))
  expect_false(is_study_name_valid(name7))
  expect_false(is_study_name_valid(name8))
  expect_false(is_study_name_valid(name9))
  expect_false(is_study_name_valid(name10))
})
