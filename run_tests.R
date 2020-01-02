library(testthat)
library(shinytest)

test_that("Application works", {
  # Don't compare images because of slight differences between
  # rending on local OS versus Travis CI.
  # Will only compare JSON representation of state.
  expect_pass(testApp(".", compareImages = FALSE))
})