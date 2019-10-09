context("test-check-team-membership.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("check_team_membership() returns check_pass if user is in the team", {
  user <- synapser::synGetUserProfile("dcctravistest")
  result <- check_team_membership(teams = "3396691", user = user)
  expect_true(inherits(result, "check_pass"))
})

test_that("check_team_membership() returns check_fail if user not in team", {
  user <- synapser::synGetUserProfile("dcctravistest")
  result <- check_team_membership(teams = "3397398", user = user)
  expect_true(inherits(result, "check_fail"))
})

test_that("check_team_membership() can check multiple teams", {
  user <- synapser::synGetUserProfile("dcctravistest")
  result <- check_team_membership(teams = c("3397398", "3377637"), user = user)
  expect_true(inherits(result, "check_fail"))
})
