context("mod-file-summary.R")

data <- tibble::tribble(
  ~x, ~y, ~z,
  "a", 1, TRUE,
  "b", 2, FALSE,
  "c", 1, FALSE
)

test_that("visualize_data_types inherits gg", {
  res <- visualize_data_types(data)
  expect_true(inherits(res, "gg"))
})

test_that("visualize_data_types returns NULL if data is wrong class", {
  res1 <- visualize_data_types(list(x = c("a", "b", "c"), y = c(1, 2, 1)))
  res2 <- visualize_data_types(1)
  res3 <- visualize_data_types(c("a", "b", "c"))
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
})

test_that("data_summary returns NULL if data is wrong class", {
  res1 <- data_summary(list(x = c("a", "b", "c"), y = c(1, 2, 1)))
  res2 <- data_summary(1)
  res3 <- data_summary(c("a", "b", "c"))
  expect_null(res1)
  expect_null(res2)
  expect_null(res3)
})

test_that("data_summary returns table with custom value column", {
  expected <- c(
    "a (1), b (1), c (1)",
    "FALSE (2), TRUE (1)",
    "1 (2), 2 (1)"
  )
  res <- data_summary(data)
  expect_true(inherits(res, "tbl_df"))
  expect_equal(res$value_occurrence, expected)
})

test_that("data_summary returns NULL if no rows in data", {
  dat <- data.frame(matrix(nrow = 0, ncol = 3))
  colnames(dat) <- c("x", "y", "z")
  res <- data_summary(dat)
  expect_null(res)
})

test_that("data_summary keeps only desired, existing columns", {
  # Desired columns from skimr plus value_occurrence
  desired_cols <- desired_cols <- c(
    "variable",
    "type",
    "missing",
    "complete",
    "n",
    "min",
    "max",
    "empty",
    "n_unique",
    "value_occurrence"
  )
  # Results in factors
  dat1 <- data.frame(
    id = c("a", "b", "c")
  )
  # Results in characters
  dat2 <- data.frame(
    id = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  # Results in numeric
  dat3 <- data.frame(
    age = c(23, 24, 24)
  )
  res1 <- data_summary(dat1)
  res2 <- data_summary(dat2)
  res3 <- data_summary(dat3)
  expect_true(all(names(res1 %in% desired_cols)))
  expect_true(all(names(res2 %in% desired_cols)))
  expect_true(all(names(res3 %in% desired_cols)))
})

test_that("data_summary returns NA if column is empty", {
  dat1 <- tibble::tribble(
    ~col1, ~col2, ~col3,
    "a", NULL, "b",
    "c", NULL, "d"
  )
  dat2 <- tibble::tribble(
    ~col1, ~col2, ~col3,
    "a", NA, "b",
    "c", NA, "d"
  )
  dat3 <- tibble::tribble(
    ~col1, ~col2, ~col3,
    "a", NULL, "b"
  )
  dat4 <- tibble::tribble(
    ~col1, ~col2, ~col3,
    "a", NA, "b"
  )
  res1 <- data_summary(dat1)
  res2 <- data_summary(dat2)
  res3 <- data_summary(dat3)
  res4 <- data_summary(dat4)
  expect_equal(
    res1$value_occurrence[res1$skim_variable == "col2"],
    as.character(NA)
  )
  expect_equal(
    res2$value_occurrence[res2$skim_variable == "col2"],
    as.character(NA)
  )
  expect_equal(
    res3$value_occurrence[res3$skim_variable == "col2"],
    as.character(NA)
  )
  expect_equal(
    res4$value_occurrence[res4$skim_variable == "col2"],
    as.character(NA)
  )
})

test_that("summarize_values returns string summary", {
  val1 <- c("a", "a", "a")
  val2 <- c("a", "b", "b")
  val3 <- c("a", "b", "c")
  val4 <- c(1, 2, 2, 1)
  res1 <- summarize_values(val1)
  res2 <- summarize_values(val2)
  res3 <- summarize_values(val3)
  res4 <- summarize_values(val4)
  expect_equal(res1, "a (3)")
  expect_equal(res2, "a (1), b (2)")
  expect_equal(res3, "a (1), b (1), c (1)")
  expect_equal(res4, "1 (2), 2 (2)")
})

test_that("summarize_values returns NA if values are all NULL or NA", {
  res1 <- summarize_values(NULL)
  res2 <- summarize_values(NA)
  res3 <- summarize_values(list(NULL, NULL, NULL))
  res4 <- summarize_values(list(NA, NA, NA))
  expect_equal(res1, NA)
  expect_equal(res2, NA)
  expect_equal(res3, NA)
  expect_equal(res4, NA)
})

test_that("get_column_definitions returns list of column definitions", {
  dat <- data.frame(
    skim_variable = "a",
    skim_type = "b",
    n_missing = 1,
    complete_rate = 0.5,
    value_occurrences = "stuff"
  )
  res <- get_column_definitions(dat)
  expect_true(inherits(res, "list"))
  expect_true(all(purrr::map_lgl(res, function(x) {
    inherits(x, "colDef")
  })))
})

test_that("get_column_definitions returns correct list of column definitions", {
  # No character or numeric data
  dat1 <- data.frame(
    skim_variable = "a",
    skim_type = "b",
    n_missing = 1,
    complete_rate = 0.5,
    value_occurrences = "stuff"
  )
  # Character data
  dat2 <- data.frame(
    skim_variable = "a",
    skim_type = "character",
    n_missing = 1,
    complete_rate = 0.5,
    character.n_unique = 1,
    value_occurrences = "stuff"
  )
  # Numeric data
  dat3 <- data.frame(
    skim_variable = "a",
    skim_type = "numeric",
    n_missing = 1,
    complete_rate = 0.5,
    numeric.mean = .8,
    numeric.sd = .01,
    numeric.hist = "histogram",
    value_occurrences = "stuff"
  )
  # Character and numeric data
  dat4 <- data.frame(
    skim_variable = c("a", "b"),
    skim_type = c("numeric", "character"),
    n_missing = c(1, 1),
    complete_rate = c(0.5, 0.5),
    character.n_unique = c(1, 1),
    numeric.mean = c(.8, .8),
    numeric.sd = c(.01, .01),
    numeric.hist = c("histogram", "otherhistogram"),
    value_occurrences = c("stuff", "morestuff")
  )
  res1 <- get_column_definitions(dat1)
  res2 <- get_column_definitions(dat2)
  res3 <- get_column_definitions(dat3)
  res4 <- get_column_definitions(dat4)
  # Columns expected based on present columns
  in_all_lists <- c(
    "skim_variable",
    "skim_type",
    "n_missing",
    "complete_rate",
    "value_occurrence"
  )
  in_character_lists <- "character.n_unique"
  in_numeric_lists <- c("numeric.mean", "numeric.sd", "numeric.hist")
  expect_true(all(names(res1) %in% in_all_lists))
  expect_true(all(names(res2) %in% c(in_all_lists, in_character_lists)))
  expect_true(all(names(res3) %in% c(in_all_lists, in_numeric_lists)))
  expect_true(all(names(res4) %in% c(
    in_all_lists,
    in_character_lists,
    in_numeric_lists
  )))
})
