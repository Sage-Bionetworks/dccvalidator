context("test-instructions.R")

test_that("instructions returns a div", {
  res <- instructions(
    annots_link = "https://test.com",
    templates_link = "https://test.com"
  )
  expect_true(inherits(res, "shiny.tag"))
  expect_equal(res$name, "div")
})

test_that("instructions contain given link", {
  res <- unlist(
    instructions(
      annots_link = "https://foo.com",
      templates_link = "https://bar.com"
    )
  )
  expect_true(any(grepl("https://foo.com", res)))
})
