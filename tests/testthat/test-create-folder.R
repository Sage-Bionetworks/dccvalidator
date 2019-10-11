context("test-create-folder.R")

library("synapser")
attempt_login()

test_that("create_folder() creates a folder", {
  skip_if_not(logged_in())

  op <- options(digits.secs = 3)
  ## Create folder name with timestamp including milliseconds
  name <- paste(
    "my_test_folder",
    format(Sys.time(), "%Y-%m-%d_%H.%M.%OS"),
    sep = "_"
  )
  options(op) # reset digits options
  created_folder <- create_folder(
    parent = "syn17038062",
    name = name
  )
  expect_true(inherits(created_folder, "Folder"))
  on.exit(synDelete(created_folder)) # delete on exit
})
