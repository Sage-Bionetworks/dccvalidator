context("test-check-ages-over-90.R")

test_that("Ages over 90 are detected", {
  dat <- data.frame(ageDeath = c(95, 100))
  res <- check_ages_over_90(dat)
  expect_true(inherits(res, "check_warn"))
})

test_that("Ages <=90 are fine", {
  dat <- data.frame(ageDeath = c(70, 90))
  res <- check_ages_over_90(dat)
  expect_true(inherits(res, "check_pass"))
})

test_that("Ages over 90 are detected in character and factor columns", {
  dat1 <- data.frame(ageDeath = c("95", "100"), stringsAsFactors = FALSE)
  dat2 <- data.frame(ageDeath = c("95", "100"), stringsAsFactors = TRUE)
  res1 <- check_ages_over_90(dat1)
  res2 <- check_ages_over_90(dat2)
  expect_true(inherits(res1, "check_warn"))
  expect_true(inherits(res2, "check_warn"))
})

test_that("check_ages_over_90 returns NULL if data is NULL", {
  expect_null(check_ages_over_90(NULL))
})

test_that("check_ages_over_90 returns check_pass if no age columns present", {
  dat <- data.frame(foo = 1)
  res <- check_ages_over_90(dat, col = "ageDeath")
  expect_true(inherits(res, "check_pass"))
})

test_that("check_ages_over_90 can check multiple columns", {
  dat <- data.frame(
    age1 = c(45, 95, 100),
    age2 = c(55, 99, 101),
    age3 = c(1, 2, 3)
  )
  res <- check_ages_over_90(dat, col = c("age1", "age2", "age3"))
  expect_true(inherits(res, "check_warn"))
  expect_equal(names(res$data), c("age1", "age2"))
})

test_that("strict flag returns failure instead of warning", {
  dat <- data.frame(ageDeath = c(95, 100))
  res <- check_ages_over_90(dat, strict = TRUE)
  expect_true(inherits(res, "check_fail"))
})

test_that("check_ages_over_90 can handle tibbles", {
  dat1 <- tibble::tibble(ageDeath = 100)
  dat2 <- tibble::tibble(age1 = 100, age2 = 50, age3 = 1000)
  res1 <- check_ages_over_90(dat1)
  res2 <- check_ages_over_90(dat2, col = c("age1", "age2", "age3"))
  expect_true(inherits(res1, "check_warn"))
  expect_true(inherits(res2, "check_warn"))
})

test_that("check_ages_over_90 doesn't fail if all NA", {
  dat1 <- data.frame(ageDeath = NA)
  dat2 <- data.frame(age1 = c(NA, NA), age2 = c(NA, NA))
  res1 <- check_ages_over_90(dat1)
  res2 <- check_ages_over_90(dat2, col = c("age1", "age2"))
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_ages_over_90 doesn't return NAs in data", {
  dat <- data.frame(ageDeath = c(NA, 95))
  res <- check_ages_over_90(dat)
  expect_equal(res$data, list(ageDeath = 95))
})

test_that("is_over_90 considers NAs not greater than 90", {
  x <- c(NA, 95, NA, 90)
  expect_equal(is_over_90(x), c(FALSE, TRUE, FALSE, FALSE))
})

test_that("is_over_90() preserves decimal ages in characters and factor", {
  x <- c(89.9, 90.9)
  y <- c("89.9", "90+", "95.5")
  z <- factor(c("99.9", "90+"))
  expect_equal(is_over_90(x), c(FALSE, TRUE))
  expect_equal(is_over_90(y), c(FALSE, FALSE, TRUE))
  expect_equal(is_over_90(z), c(TRUE, FALSE))
})

test_that("is_over_90 coerces non-character or numeric values to numeric", {
  expect_false(is_over_90(TRUE))
  expect_false(is_over_90(FALSE))
})
