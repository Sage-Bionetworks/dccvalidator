context("utils.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("on_travis() returns TRUE on Travis", {
  expect_equal(on_travis(), isTRUE(as.logical(Sys.getenv("TRAVIS"))))
})

test_that("login works on travis", {
  ## Lots of other things will fail too if it doesn't, but doesn't hurt to have
  ## a dedicated test
  if (!on_travis()) {
    skip("This test should run on Travis only")
  }
  login <- try(syn_travis_login(), silent = TRUE)
  expect_false(inherits(login, "try-error"))
})

test_that("get_annotation fails if no key provided", {
  expect_error(get_annotation("syn12345"))
})

test_that("get_annotation gets value of an annotation on a Synapse entity", {
  annot <- get_annotation("syn17038064", "fileFormat")
  expect_equal(annot, c(syn17038064 = "txt"))
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

## Check if skip occurred with given environment variables
was_skipped_with_env <- function(var, skip_fun) {
  was_skipped <- function(code) {
    tryCatch(
      # nolint start
      {
        skipped <- TRUE
        code
        skipped <- FALSE
      },
      # nolint end
      skip = function(e) NULL
    )
    return(skipped)
  }
  withr::with_envvar(var, was_skipped(skip_fun()))
}

test_that("skip_on_fork skips when owner isn't Sage-Bionetworks", {
  expect_true(
    was_skipped_with_env(
      c("TRAVIS" = "true", "TRAVIS_REPO_SLUG" = "Not-Sage/dccvalidator"),
      skip_on_fork
    )
  )
})

test_that("skip_on_fork doesn't skip when owner is Sage-Bionetworks", {
  expect_false(
    was_skipped_with_env(
      c(
        "TRAVIS" = "true",
        "TRAVIS_REPO_SLUG" = "Sage-Bionetworks/dccvalidator"
      ),
      skip_on_fork
    )
  )
})

test_that("skip_on_fork doesn't skip when not on Travis", {
  expect_false(
    was_skipped_with_env(c("TRAVIS" = NULL), skip_on_fork)
  )
})
