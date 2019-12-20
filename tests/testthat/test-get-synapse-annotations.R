context("test-get-synapse-annotations.R")

syn <- attempt_instantiate()
attempt_login(syn = syn)

test_that("get_synapse_table reads in table with empty strings as NA", {
  skip_if_not(logged_in(syn = syn))

  op <- options(digits.secs = 3)
  ## Create folder name with timestamp including milliseconds
  name <- paste(
    "my_test_table",
    format(Sys.time(), "%Y-%m-%d_%H.%M.%OS"),
    sep = "_"
  )
  options(op) # reset digits options
  dat <- data.frame(x = 1:2, y = c("", "a"), stringsAsFactors = FALSE)
  tab <- synapse$table$build_table(name, "syn17038062", dat)
  stored_table <- syn$store(tab)
  synID <- stored_table$schema$properties$id

  result <- get_synapse_table(synID, syn = syn)

  ## We're really testing two things here, but creating and then downloading a
  ## table is kind of expensive so I didn't want to split this into separate
  ## tests; separate assertions should be ok.
  expect_true(inherits(result, "data.frame"))
  expect_true(is.na(result[1, 2]))
  on.exit(syn$delete(synID)) # delete on exit
})
