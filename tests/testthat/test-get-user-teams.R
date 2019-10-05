context("test-get-user-teams.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("get_user_teams gets team memberships", {
  user <- synapser::synGetUserProfile("dcctravistest")
  teams <- get_user_teams(user)
  expect_true("3396691" %in% teams)
})
