context("test-create-folder.R")

library("synapser")
attempt_login()

test_that("create_folder() creates a folder", {
  skip_if_not(logged_in())

  created_folder <- create_folder(
    parent = "syn17038062",
    name = "my_test_folder"
  )
  expect_true(inherits(created_folder, "Folder"))
  on.exit(synDelete(created_folder)) # delete on exit
})
