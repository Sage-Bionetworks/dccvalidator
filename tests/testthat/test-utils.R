context("utils.R")

syn <- attempt_instantiate()
attempt_login(syn)

test_that("on_travis() returns TRUE on Travis", {
  expect_equal(on_travis(), isTRUE(as.logical(Sys.getenv("TRAVIS"))))
})

test_that("login works on travis in main repo", {
  ## Lots of other things will fail too if it doesn't, but doesn't hurt to have
  ## a dedicated test
  skip_if_not(on_travis())

  ## In this one we do need to ensure it only runs for builds on upstream repo,
  ## not forks
  owner <- gsub(
    "(^[^/]+)(.+)", "\\1",
    Sys.getenv("TRAVIS_PULL_REQUEST_SLUG")
  )
  skip_if_not(owner == "Sage-Bionetworks", "Testing on upstream repo")

  login <- try(syn_travis_login(syn), silent = TRUE)
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
