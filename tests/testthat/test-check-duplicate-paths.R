context("test-check-duplicate-paths.R")

test_that("check_duplicate_paths() finds duplicates", {
  dat1 <- data.frame(
    path = c("/path/to/file.txt", "/path/to/file.txt"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    path = c("/path/to/file.txt", "/path/to/file.txt"),
    stringsAsFactors = TRUE
  )
  expect_true(inherits(check_duplicate_paths(dat1), "check_fail"))
  expect_true(inherits(check_duplicate_paths(dat2), "check_fail"))
})

test_that("check_duplicate_paths() passes if no duplicates", {
  dat <- data.frame(path = c("/path/to/file1.txt", "/path/to/file2.txt"))
  expect_true(inherits(check_duplicate_paths(dat), "check_pass"))
})

test_that("check_duplicate_paths() returns NULL if data is NULL", {
  expect_null(check_duplicate_paths(NULL))
})

test_that("check_duplicate_paths() returns check_warn() if no path col", {
  dat <- data.frame(foo = 1:2)
  expect_true(inherits(check_duplicate_paths(dat), "check_warn"))
})

test_that("check_duplicate_paths() does not include NAs in resulting data", {
  dat <- data.frame(path = c("foo.txt", "foo.txt", NA, NA))
  res <- check_duplicate_paths(dat)
  expect_equal(res$data, "foo.txt")
})

test_that("check_duplicate_paths() returns check_warn if all NAs", {
  dat <- data.frame(path = c(NA, NA))
  expect_true(inherits(check_duplicate_paths(dat), "check_warn"))
})
