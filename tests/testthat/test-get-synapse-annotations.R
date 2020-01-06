context("test-get-synapse-annotations.R")

syn <- attempt_instantiate()
attempt_login(syn = syn)

test_that("get_synapse_table returns a data frame", {
  skip_if_not(logged_in(syn = syn))

  result <- get_synapse_table("syn21386611", syn = syn)
  expect_true(inherits(result, "data.frame"))
})

test_that("get_synapse_table creates NAs", {
  skip_if_not(logged_in(syn = syn))

  result <- get_synapse_table("syn21386611", syn = syn)
  expect_true(is.na(result[1, 1]))
})
