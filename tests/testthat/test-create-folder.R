context("test-create-folder.R")

syn <- attempt_instantiate()
tryCatch(
  attempt_login(syn),
  error = function(e) {
    print(glue::glue("Did not log into Synapse: {e$message}"))
  }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

test_that("create_folder() creates a folder", {
  skip_if_not(logged_in(syn = syn))

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
    name = name,
    synapseclient = synapse,
    syn = syn
  )
  expect_true(inherits(created_folder, "synapseclient.entity.Folder"))
  on.exit(syn$delete(created_folder)) # delete on exit
})
