context("test-check-cols-empty.R")

test_that("check_cols_empty returns NULL if given NULL input", {
  expect_null(check_cols_empty(NULL))
})

test_that("check_cols_empty returns check_pass if all columns have data", {
  dat <- data.frame(x = 1:2, y = c("a", "b"), z = c(TRUE, FALSE))
  res <- check_cols_empty(dat)
  expect_true(inherits(res, "check_pass"))
})

test_that("check_cols_empty returns check_warn if some columns lack data", {
  dat1 <- data.frame(x = NA)
  dat2 <- data.frame(x = "")
  dat3 <- data.frame(x = 1, y = NA, z = "")
  res1 <- check_cols_empty(dat1)
  res2 <- check_cols_empty(dat2)
  res3 <- check_cols_empty(dat3)
  expect_true(inherits(res1, "check_warn"))
  expect_true(inherits(res2, "check_warn"))
  expect_true(inherits(res3, "check_warn"))
  expect_equal(res1$data, "x")
  expect_equal(res2$data, "x")
  expect_equal(res3$data, c("y", "z"))
})

test_that("`strict` argument causes check_fail instead of check_warn", {
  dat <- data.frame(x = NA)
  res <- check_cols_empty(dat, strict = TRUE)
  expect_true(inherits(res, "check_fail"))
})

test_that("can customize empty values", {
  dat <- data.frame(x = "")
  res <- check_cols_empty(dat, empty_values = NA)
  expect_true(inherits(res, "check_pass"))
})

test_that("`required_cols` are not checked for emptiness", {
  dat <- data.frame(x = 1, y = NA, z = "")
  res1 <- check_cols_empty(dat, required_cols = c("y", "z"))
  res2 <- check_cols_empty(dat, required_cols = "y")
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_warn"))
  expect_equal(res2$data, "z")
})
