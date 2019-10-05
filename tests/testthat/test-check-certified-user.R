context("test-chec-certified-user.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("check_certified_user returns check_pass if certified", {
  res <- check_certified_user("3384770")
  expect_true(inherits(res, "check_pass"))
})

test_that("check_certified_user returns check_fail if not certified", {
  res <- check_certified_user("3397446")
  expect_true(inherits(res, "check_fail"))
})
