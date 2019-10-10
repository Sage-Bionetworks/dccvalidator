context("test-instructions.R")

test_that("instructions returns a div", {
  res <- instructions("https://test.com")
  expect_true(inherits(res, "shiny.tag"))
  expect_equal(res$name, "div")
})

test_that("instructions contain given link", {
  res <- unlist(instructions("https://foo.com"))
  expect_true(any(grepl("https://foo.com", res)))
})
