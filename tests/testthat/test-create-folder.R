context("test-create-folder.R")

library("synapser")
if (on_travis()) syn_travis_login() else synLogin()

test_that("create_folder() creates a folder", {
  created_folder <- create_folder(
    parent = "syn17038062",
    name = "my_test_folder"
  )
  expect_true(inherits(created_folder, "Folder"))
  on.exit(synDelete(created_folder)) # delete on exit
})
