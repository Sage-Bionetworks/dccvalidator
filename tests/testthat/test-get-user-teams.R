context("test-get-user-teams.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

test_that("get_user_teams gets team memberships", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  teams <- get_user_teams(user, syn)
  expect_true("3396691" %in% teams)
})
