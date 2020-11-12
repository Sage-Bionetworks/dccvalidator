context("metadata-template-dictionary.R")

# update_template_dictionaries ------------------------------------------------
syn <- attempt_instantiate()
attempt_login(syn)

test_that("update_template_dictionaries returns updated Synapse file list", {
  skip_if_not(logged_in(syn = syn))

  # Simplistic fake annotations
  annots <- tibble::tibble(
    key = c("individualID", "specimenID"),
    description = c("ID of individual", "ID of specimen"),
    value = c(NA, NA),
    valueDescription = c(NA, NA),
    source = c(NA, NA)
  )

  # Two AD templates, biospecimen and rnaSeq assay
  temps <- c("syn12973252", "syn12973256")

  res <- update_template_dictionaries(
    templates = temps,
    annotations = annots,
    syn
  )

  # Returns as list with 2 elements
  expect_true(inherits(res, "list"))
  expect_equal(length(res), 2)

  # File dictionaries have been overwritten correctly
  temp1_description <- readxl::read_xlsx(res[[1]]$path, sheet = 2)
  temp1_values <- readxl::read_xlsx(res[[1]]$path, sheet = 3)
  temp2_description <- readxl::read_xlsx(res[[2]]$path, sheet = 2)
  temp2_values <- readxl::read_xlsx(res[[2]]$path, sheet = 3)

  expect_equal(temp1_description, annots[, c("key", "description")])
  expect_equal(
    temp1_values,
    annots[, c("key", "value", "valueDescription", "source")]
  )
  expect_equal(temp2_description, annots[2, c("key", "description")])
  expect_equal(
    temp2_values,
    annots[2, c("key", "value", "valueDescription", "source")]
  )

  # Clean up local files
  file.remove(list.files(".", pattern = "^template(.+)\\.xlsx"))
})

# verify_dictionary_structure -------------------------------------------------

test_that("verify_dictionary_structure throws error if no dictionary", {
  expect_error(verify_dictionary_structure(dictionary = NA))
  expect_error(verify_dictionary_structure(dictionary = NA))
})

test_that("verify_dictionary_structure throws error if not a data frame", {
  # Spot check with list, vector, boolean
  dat1 <- list(
    key = c("foo"),
    description = c("bar"),
    columnType = c("baz")
  )
  dat2 <- c(1, 2, 3)
  dat3 <- TRUE
  expect_error(verify_dictionary_structure(dat1))
  expect_error(verify_dictionary_structure(dat2))
  expect_error(verify_dictionary_structure(dat3))
})

test_that("verify_dictionary_structure error message is correct if missing columns", { # nolint 
  dat <- data.frame(
    key = c("foo", "bar"),
    description = c("foo", "bar"),
    columnType = c("foo", "bar"),
    stringsAsFactors = FALSE
  )
  # Missing key
  res1 <- expect_error(
    verify_dictionary_structure(
      dictionary = dat[, c("description", "columnType")]
    )
  )
  expect_equal(res1$message, "Dictionary is missing the column(s): key")

  # Missing description
  res2 <- expect_error(
    verify_dictionary_structure(
      dictionary = dat[, c("key", "columnType")]
    )
  )
  expect_equal(res2$message, "Dictionary is missing the column(s): description")

  # Missing columnType
  res3 <- expect_error(
    verify_dictionary_structure(
      dictionary = dat[, c("key", "description")]
    )
  )
  expect_equal(res3$message, "Dictionary is missing the column(s): columnType")

  # Missing key and description
  res4 <- expect_error(
    verify_dictionary_structure(
      dictionary = dat[, "columnType", drop = FALSE]
    )
  )
  expect_equal(
    res4$message,
    "Dictionary is missing the column(s): key, description"
  )
})

test_that("verify_dictionary_structure throws error if > 1 key description", {
  dat1 <- data.frame(
    key = c("foo", "foo"),
    description = c("bar", "baz"),
    columnType = c("boo", "boo"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    key = c("foo", "foo"),
    description = c(NA, "baz"),
    columnType = c("boo", "boo"),
    stringsAsFactors = FALSE
  )
  dat3 <- data.frame(
    key = c("foo", "foo", "bar", "bar"),
    description = c("foo", "bar", "foo", "bar"),
    columnType = c("baz", "baz", "baz", "baz"),
    stringsAsFactors = FALSE
  )
  res1 <- verify_dictionary_structure(dat1)
  res2 <- verify_dictionary_structure(dat2)
  res3 <- verify_dictionary_structure(dat3)
  expect_true(inherits(res1, "check_fail"))
  expect_equal(res1$data, "foo")
  expect_true(inherits(res2, "check_fail"))
  expect_equal(res2$data, "foo")
  expect_true(inherits(res3, "check_fail"))
  expect_equal(res3$data, c("bar", "foo"))
})

test_that("verify_dictionary_structure throws error if > 1 key columnType", {
  dat1 <- data.frame(
    key = c("foo", "foo"),
    description = c("bar", "bar"),
    columnType = c("boo", "baz"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    key = c("foo", "foo"),
    description = c("bar", "bar"),
    columnType = c("boo", NA),
    stringsAsFactors = FALSE
  )
  dat3 <- data.frame(
    key = c("foo", "foo", "bar", "bar"),
    description = c("bar", "bar", "foo", "foo"),
    columnType = c("baz", "boo", "baz", NA),
    stringsAsFactors = FALSE
  )
  res1 <- verify_dictionary_structure(dat1)
  res2 <- verify_dictionary_structure(dat2)
  res3 <- verify_dictionary_structure(dat3)
  expect_true(inherits(res1, "check_fail"))
  expect_equal(res1$data, "foo")
  expect_true(inherits(res2, "check_fail"))
  expect_equal(res2$data, "foo")
  expect_true(inherits(res3, "check_fail"))
  expect_equal(res3$data, c("bar", "foo"))
})

test_that("verify_dictionary_structure returns check_pass with no data", {
  dat1 <- data.frame(
    key = c("foo", "foo"),
    description = c("bar", "bar"),
    columnType = c("baz", "baz"),
    stringsAsFactors = FALSE
  )
  dat2 <- data.frame(
    key = c("foo", "foo", "bar"),
    description = c("bar", "bar", "foo"),
    columnType = c("baz", "baz", "baz"),
    stringsAsFactors = FALSE
  )
  dat3 <- data.frame(
    key = c("foo", "foo", "baz", "baz"),
    description = c("bar", "bar", "foo", "foo"),
    columnType = c("baz", "baz", NA, NA),
    stringsAsFactors = FALSE
  )
  res1 <- verify_dictionary_structure(dat1)
  res2 <- verify_dictionary_structure(dat2)
  res3 <- verify_dictionary_structure(dat3)
  expect_true(inherits(res1, "check_pass"))
  expect_true(is.null(res1$data))
  expect_true(inherits(res2, "check_pass"))
  expect_true(is.null(res2$data))
  expect_true(inherits(res3, "check_pass"))
  expect_true(is.null(res3$data))
})

# generate_key_description ----------------------------------------------------

test_that("generate_key_description errors if missing columns or annots", {
  # source column unnecessary for this function, but allows for testing
  # with both columns missing
  dat <- data.frame(
    key = c("foo", "foo", "bar", "bar", "bar", "baz"),
    description = c("boo", "boo", "moo", "moo", "moo", "shoo"),
    source = c(NA, NA, "my mom", "your mom", "their mom", "tim"),
    stringsAsFactors = FALSE
  )
  # Missing key
  expect_error(generate_key_description(dat[, c("description", "source")]))
  # Missing description
  expect_error(generate_key_description(dat[, c("key", "source")]))
  # Missing both
  expect_error(generate_key_description(data[, "source", drop = FALSE]))
  # Missing annots
  expect_error(generate_key_description(annots = NA))
  expect_error(generate_key_description(annots = NULL))
})

test_that("generate_key_description returns description set", {
  # source column unnecessary for this function, but allows for testing
  # that extra column data isn't returned
  # Also added in an NA description to make sure that returns correctly
  dat <- data.frame(
    key = c("foo", "foo", "bar", "bar", "bar", "baz"),
    description = c("boo", "boo", "moo", "moo", "moo", NA),
    source = c(NA, NA, "my mom", "your mom", "their mom", "tim"),
    stringsAsFactors = FALSE
  )
  expected <- tibble::tibble(
    key = c("bar", "baz", "foo"),
    description = c("moo", NA, "boo")
  )
  res <- generate_key_description(annots = dat)
  expect_equal(res, expected)
})

# add_dictionary_sheets -------------------------------------------------------

test_that("add_dictionary_sheets returns error if missing columns or annots", {
  dat <- data.frame(
    key = c("foo", "foo", "bar", "bar", "bar", "baz"),
    description = c("boo", "boo", "moo", "moo", "moo", NA),
    value = c(NA, NA, "that one", "this one", "the other one", "cereal"),
    valueDescription = c(NA, NA, "that bar", "this bar", "other bar", "yum"),
    source = c(NA, NA, "my mom", "your mom", "their mom", "tim"),
    stringsAsFactors = FALSE
  )
  res1 <- expect_error(add_dictionary_sheets(
    annotations = dat[, c("key", "description")]
  ))
  # Just check first error message
  expect_equal(
    res1$message,
    "Annotations are missing the column(s): value, valueDescription, source"
  )
  # Check a few other possible combinations
  expect_error(add_dictionary_sheets(
    annotations = dat[, c("key", "description", "source")]
  ))
  expect_error(add_dictionary_sheets(
    annotations = data[,  c("value", "valueDescription")]
  ))
  # NULL/NA won't have a custom error message but should still be checked
  expect_error(add_dictionary_sheets(annotations = NA))
  expect_error(add_dictionary_sheets(annotations = NULL))
})

test_that("add_dictionary_sheets creates correct sheet list", {
  # Set up annotations and template
  annots <- data.frame(
    key = c("foo", "bar", "bar", "bar", "baz"),
    description = c("boo", "moo", "moo", "moo", NA),
    value = c(NA, "that one", "this one", "the other one", "cereal"),
    valueDescription = c(NA, "that bar", "this bar", "other bar", "yum"),
    source = c(NA, "my mom", "your mom", "their mom", "tim"),
    stringsAsFactors = FALSE
  )
  temp <- data.frame(foo = NA, bar = NA)

  # Expected dictionary tables
  dictionary <- tibble::tibble(
    key = c("bar", "foo"),
    description = c("moo", "boo")
  )
  values <- data.frame(
    key = c("foo", "bar", "bar", "bar"),
    value = c(NA, "that one", "this one", "the other one"),
    valueDescription = c(NA, "that bar", "this bar", "other bar"),
    source = c(NA, "my mom", "your mom", "their mom"),
    stringsAsFactors = FALSE
  )

  # Mock reading and writing the files
  # Need stubbed function to return template
  mockery::stub(add_dictionary_sheets, "readxl::read_xlsx", temp)
  # Mock function to primarily capture args sent to writexl
  mocked <- mockery::mock(TRUE, cycle = TRUE)
  mockery::stub(add_dictionary_sheets, "writexl::write_xlsx", mocked)

  res <- add_dictionary_sheets(
    template_xlsx_path = "my_fake_path.xlsx",
    annotations = annots
  )
  # Quick check that add_dictionary sheets returned path
  expect_equal(res, "my_fake_path.xlsx")
  # Get list sent to writexl
  args <- mockery::mock_args(mocked)[[1]][[1]]
  expect_equal(args$template, temp)
  expect_equal(args$dictionary, dictionary)
  expect_equal(args$values, values)
})

# get_template_synIDs ---------------------------------------------------------

test_that("get_template_synIDs returns vector of synIDs from list", {
  # Named, nested list
  dat1 <- list(
    template1 = "syn111111",
    template2 = "syn222222",
    template_set = list(
      template3 = "syn333333",
      template4 = "syn444444"
    )
  )
  # Unnamed, nested list
  dat2 <- list(
    "syn111111",
    "syn222222",
    list(
      "syn333333",
      "syn444444"
    )
  )
  # Single ID
  dat3 <- list("syn111111")

  res1 <- get_template_synIDs(dat1)
  res2 <- get_template_synIDs(dat2)
  res3 <- get_template_synIDs(dat3)

  expected <- c("syn111111", "syn222222", "syn333333", "syn444444")
  expect_equal(res1, expected)
  expect_equal(res2, expected)
  expect_equal(res3, "syn111111")
})

test_that("get_template_synIDs returns vector of synIDs from config", {
  # config.yml at top level of /tests
  expected <- c(
    "syn12973254",
    "syn12973253",
    "syn12973252",
    "syn12973252",
    "syn20673251",
    "syn12973256",
    "syn12973255",
    "syn20820080"
  )
  expect_equal(get_template_synIDs(), expected)
})
