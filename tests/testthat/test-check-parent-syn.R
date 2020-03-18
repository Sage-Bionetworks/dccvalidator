context("test-parent-syn.R")

test_that("check_parent_syn() looks for 'syn' + 1 or more digits", {
  dat1 <- data.frame(parent = "foo")
  dat2 <- data.frame(parent = "syn")
  dat3 <- data.frame(parent = "syn1")
  dat4 <- data.frame(parent = "syn123")
  res1 <- check_parent_syn(dat1)
  res2 <- check_parent_syn(dat2)
  res3 <- check_parent_syn(dat3)
  res4 <- check_parent_syn(dat4)
  expect_true(inherits(res1, "check_fail"))
  expect_true(inherits(res2, "check_fail"))
  expect_true(inherits(res3, "check_pass"))
  expect_true(inherits(res4, "check_pass"))
})

test_that("check_parent_syn() works if there are NAs", {
  dat <- data.frame(parent = c("syn", "syn123", NA), stringsAsFactors = FALSE)
  res <- check_parent_syn(dat)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, c("syn", NA))
})

test_that("check_parent_syn() returns check_fail if no parent column", {
  dat <- data.frame(path = "foo")
  res <- check_parent_syn(dat)
  expect_true(inherits(res, "check_fail"))
})
