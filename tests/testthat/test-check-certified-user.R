context("test-chec-certified-user.R")

library("synapser")
attempt_login()

test_that("check_certified_user returns check_pass if certified", {
  skip_if_not(logged_in())

  res <- check_certified_user("3384770")
  expect_true(inherits(res, "check_pass"))
})

test_that("check_certified_user returns check_fail if not certified", {
  skip_if_not(logged_in())

  res <- check_certified_user("3397446")
  expect_true(inherits(res, "check_fail"))
})
