context("test-python-helpers.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

test_that("dict_to_list converts annotations", {
  skip_if_not(logged_in(syn = syn))
  # I haven't been able to recreate a Python object by hand that matches what's
  # returned from Synapse, so we'll just test with real annotations
  x <- syn$getAnnotations("syn17038064")
  expect_equal(dict_to_list(x), list(fileFormat = "txt"))
})
