context("test-chec-certified-user.R")

library("reticulate")
use_python("usr/local/bin/python3")
synapse <- reticulate::import("synapseclient")
syn <- synapse$Synapse()
attempt_login(syn)

test_that("check_certified_user returns check_pass if certified", {
  skip_if_not(logged_in(syn = syn))

  res <- check_certified_user("3384770", syn)
  expect_true(inherits(res, "check_pass"))
})

test_that("check_certified_user returns check_fail if not certified", {
  skip_if_not(logged_in(syn = syn))

  res <- check_certified_user("3397446", syn)
  expect_true(inherits(res, "check_fail"))
})
