context("test-df-to-json.R")

test_that("df_to_json_list converts one row", {
  dat <- data.frame(x = 1)
  result <- df_to_json_list(dat)
  expect_equal(result, list(structure("{\"x\":1}", class = "json")))
})

test_that("df_to_json_list converts two rows", {
  dat <- data.frame(x = c("a", "b"))
  result <- df_to_json_list(dat)
  expect_equal(
    result,
    list(
      structure("{\"x\":\"a\"}", class = "json"),
      structure("{\"x\":\"b\"}", class = "json")
    )
  )
})

test_that("df_to_json_list converts multiple columns", {
  dat <- data.frame(x = 1:2, y = c(TRUE, FALSE))
  result <- df_to_json_list(dat)
  expect_equal(
    result,
    list(
      structure("{\"x\":1,\"y\":true}", class = "json"),
      structure("{\"x\":2,\"y\":false}", class = "json")
    )
  )
})

test_that("df_to_json_list removes NA values", {
  dat <- data.frame(x = NA)
  result <- df_to_json_list(dat)
  expect_equal(result, list(structure("{}", class = "json")))
})

test_that("df_to_json_list works on tibbles", {
  dat1 <- data.frame(x = 1)
  dat2 <- tibble::as_tibble(dat1)
  expect_equal(df_to_json_list(dat1), df_to_json_list(dat2))

  dat3 <- data.frame(x = 1:2, y = c("a", "b"))
  dat4 <- tibble::as_tibble(dat3)
  expect_equal(df_to_json_list(dat3), df_to_json_list(dat4))
})
