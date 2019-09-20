context("test-get-user-teams.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("get_user_teams gets team memberships", {
  teams <- get_user_teams("dcctravistest")
  expect_equal(teams, "3396691")
})
