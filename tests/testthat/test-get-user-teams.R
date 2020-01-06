context("test-get-user-teams.R")

syn <- attempt_instantiate()
attempt_login(syn)

test_that("get_user_teams gets team memberships", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  teams <- get_user_teams(user, syn)
  expect_true("3396691" %in% teams)
})
