context("utils.R")

test_that("on_ci() returns TRUE on CI", {
  expect_equal(on_ci(), isTRUE(as.logical(Sys.getenv("CI"))))
})

test_that("login works on CI in main repo", {
  ## Lots of other things will fail too if it doesn't, but doesn't hurt to have
  ## a dedicated test
  skip_if_not(on_ci())

  ## In this one we do need to ensure it only runs for builds on upstream repo,
  ## not forks; forks may not have the necessary secrets for the test
  owner <- Sys.getenv("GITHUB_ACTOR")
  skip_if_not(owner == "Sage-Bionetworks", "Testing on upstream repo")

  syn <- attempt_instantiate()
  # Check that the client was instantiated or will not get correct error
  expect_true(inherits(syn, "synapseclient.client.Synapse"))
  login <- try(attempt_login(syn), silent = TRUE)
  expect_false(inherits(login, "try-error"))
})

test_that("%||% gives b if a is NULL", {
  a <- NULL
  b <- "foo"
  expect_equal(a %||% b, "foo")

  a <- "bar"
  b <- "baz"
  expect_equal(a %||% b, "bar")

  a <- NA
  b <- "foo"
  expect_equal(a %||% b, NA)

  a <- NULL
  b <- NULL
  expect_null(a %||% b)
})

test_that("count_unique_values returns correct number", {
  expect_equal(count_unique_values(1, 2, 3), 3)
  expect_equal(count_unique_values(1, 1, 1), 1)
  expect_equal(count_unique_values("a"), 1)
  # Should not count NA as a unique value
  expect_equal(count_unique_values(NA), 0)
  expect_equal(count_unique_values("a", NA, NA), 1)
  expect_equal(count_unique_values(c(NA), c(1, 2), c("a", "b")), 4)
})
