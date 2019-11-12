context("test-app-reporting.R")

library("purrr")

# report_result() --------------------------------------------------------------

# Some sample data
dat <- check_fail(
  msg = "this thing failed",
  behavior = "it should have passed",
  data = c("thing1", "thing2")
)
res <- report_result(dat, "x")

test_that("report_result returns a div", {
  expect_true(inherits(res, "shiny.tag"))
  expect_equal(res$name, "div")
})

test_that("report_result includes message indicating what went wrong", {
  expect_equal(res$children[[1]]$children[[2]], "this thing failed")
})

test_that("report_result `verbose` option adds data", {
  res2 <- report_result(dat, "x", verbose = TRUE)
  expect_equal(
    res2$children[[1]]$children[[3]]$children[[2]],
    "thing1, thing2" # report_results concatenates the vector
  )
})

# report_results() -------------------------------------------------------------

test_that("report_results creates results for multiple objects", {
  # List of check results
  dat_list <- list(
    check_pass(msg = "it passed", behavior = "it should be good", data = NULL),
    check_warn(msg = "it's a warning", behavior = "it should rock", data = 1:3)
  )
  res_list <- report_results(dat_list, emoji_prefix = "apple", verbose = TRUE)

  expect_equal(length(res_list), 2)
  expect_true(all(map_lgl(res_list, function(x) x$name == "div")))
})

# show_details() ---------------------------------------------------------------

test_that("show_details concatenates a vector of data", {
  expect_equal(show_details(c("a", "b", "c")), "a, b, c")
  expect_equal(show_details(1:3), "1, 2, 3")
  expect_equal(show_details(NULL), "")
  expect_equal(show_details(c(NA, "x")), "NA, x")
})

test_that("show_details creates a render function from a list of data", {
  dat <- list(A = 1:3, B = c("red", "green", "blue"))
  res <- show_details(dat)
  expect_true(inherits(res, "shiny.render.function"))
})
