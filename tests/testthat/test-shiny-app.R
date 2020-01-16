context("test-shiny-app-function")

library(shinytest)

test_that("Shiny app works", {
  skip_on_cran()

  expect_pass(testApp("apps/", compareImages = FALSE))
})