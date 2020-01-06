context("test-check-team-membership.R")

syn <- attempt_instantiate()
attempt_login(syn)

test_that("check_team_membership() returns check_pass if user is in the team", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  result <- check_team_membership(teams = "3396691", user = user, syn = syn)
  expect_true(inherits(result, "check_pass"))
})

test_that("check_team_membership() returns check_fail if user not in team", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  result <- check_team_membership(teams = "3397398", user = user, syn = syn)
  expect_true(inherits(result, "check_fail"))
})

test_that("check_team_membership() can check multiple teams", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  result <- check_team_membership(
    teams = c("3397398", "3377637"),
    user = user,
    syn = syn
  )
  expect_true(inherits(result, "check_fail"))
})
