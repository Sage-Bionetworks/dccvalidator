context("conditions.R")

test_that("check_pass creates check_pass object that inherits from message", {
  res <- check_pass(msg = "foo", behavior = "bar")
  expect_true(inherits(res, "check_pass"))
  expect_true(inherits(res, "message"))
})

test_that("check_warn creates check_warn object that inherits from warning", {
  res <- check_warn(msg = "foo", behavior = "bar", data = 1)
  expect_true(inherits(res, "check_warn"))
  expect_true(inherits(res, "warning"))
})

test_that("check_fail creates check_fail object that inherits from error", {
  res <- check_fail(msg = "foo", behavior = "bar", data = 1)
  expect_true(inherits(res, "check_fail"))
  expect_true(inherits(res, "error"))
})

test_that("data is available from condition objects", {
  pass <- check_pass("foo", "bar", 1)
  warn <- check_warn("foo", "bar", 2)
  fail <- check_fail("foo", "bar", 3)
  expect_equal(pass$data, 1)
  expect_equal(warn$data, 2)
  expect_equal(fail$data, 3)
})

test_that("check_condition can switch between check types", {
  pass <- check_condition("foo", "bar", "baz", "check_pass")
  warn <- check_condition("foo", "bar", "baz", "check_warn")
  fail <- check_condition("foo", "bar", "baz", "check_fail")
  expect_true(inherits(pass, "check_pass"))
  expect_true(inherits(warn, "check_warn"))
  expect_true(inherits(fail, "check_fail"))
})
