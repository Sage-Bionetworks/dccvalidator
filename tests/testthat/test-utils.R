context("utils.R")

test_that("on_travis() returns TRUE on Travis", {
  expect_equal(on_travis(), isTRUE(as.logical(Sys.getenv("TRAVIS"))))
})

test_that("login works on travis", {
  ## Lots of other things will fail too if it doesn't, but doesn't hurt to have
  ## a dedicated test
  if (!on_travis()) { # only run on travis
    skip("This test should run on Travis only")
  }
  login <- try(syn_travis_login(), silent = TRUE)
  expect_false(inherits(login, "try-error"))
})
