context("test-get-user-teams.R")

library("reticulate")
use_python("usr/local/bin/python3")
synapse <- reticulate::import("synapseclient")
syn <- synapse$Synapse()
attempt_login(syn)

test_that("get_user_teams gets team memberships", {
  skip_if_not(logged_in(syn = syn))

  user <- syn$getUserProfile("dcctravistest")
  teams <- get_user_teams(user, syn)
  expect_true("3396691" %in% teams)
})
