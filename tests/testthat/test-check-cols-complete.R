context("test-check-cols-complete.R")

test_that("check_cols_complete returns NULL if given NULL input", {
  expect_null(check_cols_complete(NULL))
})

test_that("check_cols_complete returns check_pass if required columns are complete or missing", { # nolint
  dat1 <- data.frame(x = 1:2, y = c("a", "b"), z = c(TRUE, FALSE))
  dat2 <- data.frame(x = 1)
  res1 <- check_cols_complete(dat1, c("x", "y", "z"))
  res2 <- check_cols_complete(dat2, "y")
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_cols_complete returns check_fail if any required column lacks data", { # nolint
  dat1 <- data.frame(x = NA)
  dat2 <- data.frame(x = "")
  dat3 <- data.frame(x = 1, y = NA, z = "")
  res1 <- check_cols_complete(dat1, "x")
  res2 <- check_cols_complete(dat2, "x")
  res3 <- check_cols_complete(dat3, c("x", "y", "z"))
  expect_true(inherits(res1, "check_fail"))
  expect_true(inherits(res2, "check_fail"))
  expect_true(inherits(res3, "check_fail"))
  expect_equal(res1$data, "x")
  expect_equal(res2$data, "x")
  expect_equal(res3$data, c("y", "z"))
})

test_that("`strict` argument causes check_warn instead of check_fail", {
  dat <- data.frame(x = NA)
  res <- check_cols_complete(dat, "x", strict = FALSE)
  expect_true(inherits(res, "check_warn"))
})

test_that("can customize empty values", {
  dat <- data.frame(x = "")
  res <- check_cols_complete(dat, "x", empty_values = NA)
  expect_true(inherits(res, "check_pass"))
})
