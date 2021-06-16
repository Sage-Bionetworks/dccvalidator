context("test-get-synapse-annotations.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

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
