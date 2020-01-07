context("test-check-files-manifest.R")

library("tibble")

test_that("check_files_manifest returns NULL if NULL manifest", {
  res <- check_files_manifest(NULL, "test.csv")
  expect_null(res)
})

test_that("check_files_manifest returns NULL if NULL filenames", {
  manifest <- data.frame(path = c("a.csv", "b.txt"))
  res1 <- check_files_manifest(manifest, NULL)
  res2 <- check_files_manifest(manifest, c(NULL, NULL, NULL))
  expect_null(res1)
  expect_null(res2)
})

test_that("check_files_manifest returns check_fail if no path column", {
  manifest <- data.frame(x = 1:5)
  res <- check_files_manifest(manifest, "test.csv")
  expect_true(inherits(res, "check_fail"))
})

test_that("check_files_manifest returns check_warn if files are missing", {
  manifest <- data.frame(path = c("a.csv", "b.txt"), stringsAsFactors = FALSE)
  res <- check_files_manifest(manifest, "test.csv")
  expect_true(inherits(res, "check_warn"))
  expect_equal(res$data, "test.csv")
})

test_that("check_files_manifest returns check_fail if strict = TRUE", {
  manifest <- data.frame(path = c("a.csv", "b.txt"), stringsAsFactors = FALSE)
  res <- check_files_manifest(manifest, "test.csv", strict = TRUE)
  expect_true(inherits(res, "check_fail"))
  expect_equal(res$data, "test.csv")
})

test_that("check_files_manifest works if path column is a factor", {
  manifest <- data.frame(path = c("a.csv", "b.txt"))
  res <- check_files_manifest(manifest, "test.csv")
  expect_true(inherits(res, "check_warn"))
  expect_equal(res$data, "test.csv")
})

test_that("check_files_manifest returns check_pass if files are present", {
  manifest <- data.frame(path = c("a.csv", "b.txt", "c.docx"))
  res1 <- check_files_manifest(manifest, c("a.csv"))
  res2 <- check_files_manifest(manifest, c("a.csv", "b.txt", "c.docx"))
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_pass"))
})

test_that("check_files_manifest works with tibbles", {
  manifest <- tibble(path = c("a.csv", "b.txt", "c.docx"))
  res1 <- check_files_manifest(manifest, c("a.csv"))
  res2 <- check_files_manifest(manifest, c("d.xlsx"))
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_warn"))
})

test_that("check_files_manifest avoids partial matching", {
  manifest <- data.frame(
    pathway = c("a.csv", "b.txt"),
    path = c("c.docx", "d.xlsx")
  )
  res <- check_files_manifest(manifest, "a.csv")
  expect_true(inherits(res, "check_warn"))
  expect_equal(res$data, "a.csv")
})

test_that("filenames can contain NULL", {
  manifest <- data.frame(path = c("a.csv", "b.txt"))
  res <- check_files_manifest(
    manifest,
    filenames = c("a.csv", NULL, "b.txt")
  )
  expect_true(inherits(res, "check_pass"))
})

test_that("manifest can have full paths", {
  manifest <- data.frame(
    path = c(
      file.path("", "Users", "me", "file.csv"),
      file.path("~", "me", "file.docx")
    )
  )
  res1 <- check_files_manifest(manifest, "file.csv")
  res2 <- check_files_manifest(manifest, "file.txt")
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_warn"))
})
