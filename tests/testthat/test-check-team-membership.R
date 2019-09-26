context("test-check-team-membership.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("check_team_membership() does nothing if user is in the team", {
  user <- synapser::synGetUserProfile("dcctravistest")
  user_teams <- get_user_teams(user)
  result <- check_team_membership(team = "3396691", user_teams = user_teams)
  expect_null(result)
})

## TODO: test the behavior when a user *is* in the team. This requires a shiny
## session in which to show the dialog. It'll be something to address as part of
## https://github.com/Sage-Bionetworks/dccvalidator/issues/56 -- how to test the
## app itself. We may want to make some mini apps with some of these components
## for testing.
