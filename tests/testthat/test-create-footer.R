context("test-create-footer.R")

test_that("create_footer creates footer", {
  foot <- create_footer("test@test.com")
  expect_true(inherits(foot, "shiny.tag"))
  expect_equal(foot$name, "footer")
})

test_that("footer contains email", {
  res <- unlist(create_footer(email = "foo@bar.com"))
  expect_true(any(grepl("mailto:foo@bar.com", res)))
})
