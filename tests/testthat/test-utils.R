context("utils.R")

syn <- attempt_instantiate()
attempt_login(syn)

test_that("on_ci() returns TRUE on Travis", {
  expect_equal(on_ci(), isTRUE(as.logical(Sys.getenv("CI"))))
})

test_that("login works on travis in main repo", {
  ## Lots of other things will fail too if it doesn't, but doesn't hurt to have
  ## a dedicated test
  skip_if_not(on_ci())

  ## In this one we do need to ensure it only runs for builds on upstream repo,
  ## not forks
  owner <- gsub(
    "(^[^/]+)(.+)", "\\1",
    Sys.getenv("TRAVIS_PULL_REQUEST_SLUG")
  )
  skip_if_not(owner == "Sage-Bionetworks", "Testing on upstream repo")

  login <- try(syn_ci_login(syn), silent = TRUE)
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

test_that("remove_empty_rows removes rows that are all NA", {
  dat1 <- tibble::tibble(x = c(1, NA, NA), y = c(1, 2, NA))
  dat2 <- tibble::tibble(x = c(NA, NA), y = c(NA), NA)
  dat3 <- tibble::tibble(x = c("a", "b"), y = c("c", "d"))

  expect_equal(remove_empty_rows(dat1), dat1[1:2, ])
  expect_equal(nrow(remove_empty_rows(dat2)), 0)
  expect_equal(remove_empty_rows(dat3), dat3)
})
