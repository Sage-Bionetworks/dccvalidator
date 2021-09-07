context("check-invalid-characters.R")

test_that("check_invalid_characters returns check_pass if valid", {
  dat <- tibble::tibble(
    study = c("study 1", "study-2", "study_3"),
    path = c("file.ext", "dir/file.ext", "drive:dir\\xdir\\file.ext")
  )
  expect_true(inherits(
    check_invalid_characters(dat),
    "check_pass"
  ))
})

test_that("check_invalid_characters returns check_fail if invalid", {
  ## Non-ascii
  dat1 <- tibble::tibble(
    study = c("study 1", "study\xF02", "study_3"),
    path = c("file.ext", "dir/file.ext", "drive:dir\xdir\file.ext")
  )
  expect_true(inherits(
    check_invalid_characters(dat1),
    "check_fail"
  ))
  ## Oddities that start with <0x
  dat2 <- tibble::tibble(
    study = c("study 1", "study-02", "study_3"),
    path = c("fi<0xa0>le.ext", "dir/file.ext", "drive:dir\xdir\file.ext")
  )
  expect_true(inherits(
    check_invalid_characters(dat2),
    "check_fail"
  ))
  ## Oddities that start with & and end with ;
  dat3 <- tibble::tibble(
    study = c("study 1", "study-02", "study_3"),
    path = c("fi&quote;le.ext", "dir/file.ext", "drive:dir\xdir\file.ext")
  )
  expect_true(inherits(
    check_invalid_characters(dat3),
    "check_fail"
  ))
  ## Oddities that start with &# and end with ;
  dat4 <- tibble::tibble(
    study = c("study 1", "study-02", "study&#168;3"),
    path = c("file.ext", "dir/file.ext", "drive:dir\xdir\file.ext")
  )
  expect_true(inherits(
    check_invalid_characters(dat4),
    "check_fail"
  ))
})

test_that("check_invalid_characters returns column name if invalid value", {
  ## One column
  dat1 <- tibble::tibble(
    study = c("study 1", "study\xF02", "study_3"),
    path = c("file.ext", "dir/file.ext", "drive:dir\\dir\\file.ext")
  )
  res1 <- check_invalid_characters(dat1)
  expect_equal(res1$data, "study")
  ## Two columns
  dat2 <- tibble::tibble(
    study = c("study 1", "study\xF02", "study_3"),
    path = c("fi<0xa0>le.ext", "dir/file.ext", "drive:dir\xdir\file.ext")
  )
  res2 <- check_invalid_characters(dat2)
  expect_equal(res2$data, c("study", "path"))
})

test_that("contains_invalid returns true for typical invalid characters", {
  expect_true(contains_invalid("<0x00>"))
  expect_true(contains_invalid("<0x"))
  expect_true(contains_invalid("&quote;"))
  expect_true(contains_invalid("&#163;"))
  expect_true(contains_invalid("\xF0"))
})

test_that("contains_invalid returns false for valid characters", {
  expect_false(contains_invalid("foo"))
  expect_false(contains_invalid("foo-bar"))
  expect_false(contains_invalid("foo_bar"))
  expect_false(contains_invalid("&"))
  expect_false(contains_invalid("& foo"))
  expect_false(contains_invalid("foo\\xbar"))
})
