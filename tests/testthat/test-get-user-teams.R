context("test-get-user-teams.R")

library("synapser")
attempt_login()

test_that("get_user_teams gets team memberships", {
  skip_if_not(logged_in())

  user <- synapser::synGetUserProfile("dcctravistest")
  teams <- get_user_teams(user)
  expect_true("3396691" %in% teams)
})
