context("test-check-duplicate-files.R")

test_that("check_duplicate_files() finds duplicates", {
  dat1 <- data.frame(
    path = c("/path/to/file.txt", "/path/to/file.txt"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    path = c("/path/to/file.txt", "/path/to/file.txt"),
    stringsAsFactors = TRUE
  )
  expect_true(inherits(check_duplicate_files(dat1), "check_fail"))
  expect_true(inherits(check_duplicate_files(dat2), "check_fail"))
})

test_that("check_duplicate_files() passes if no duplicates", {
  dat <- data.frame(path = c("/path/to/file1.txt", "/path/to/file2.txt"))
  expect_true(inherits(check_duplicate_files(dat), "check_pass"))
})

test_that("check_duplicate_files() returns NULL if data is NULL", {
  expect_null(check_duplicate_files(NULL))
})

test_that("check_duplicate_files() returns check_warn() if no path col", {
  dat <- data.frame(foo = 1:2)
  expect_true(inherits(check_duplicate_files(dat), "check_warn"))
})
