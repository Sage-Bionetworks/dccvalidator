context("test-check-schema-df.R")

test_that("check_schema_df passes if all rows valid", {
  skip_if_not_installed("jsonvalidate")
  dat <- data.frame(x = 1:2)
  schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    }
  },
  "required": ["x"]
}
'
  result <- check_schema_df(dat, schema)
  expect_true(inherits(result, "check_pass"))
})

test_that("check_schema_df fails if all rows invalid", {
  skip_if_not_installed("jsonvalidate")
  dat <- data.frame(x = c("a", "b"))
  schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    }
  },
  "required": ["x"]
}
'
  result <- check_schema_df(dat, schema)
  expect_true(inherits(result, "check_fail"))
})

test_that("check_schema_df fails if some rows invalid", {
  skip_if_not_installed("jsonvalidate")
  dat <- data.frame(x = c(1, NA))
  schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    }
  },
  "required": ["x"]
}
'
  result <- check_schema_df(dat, schema)
  expect_true(inherits(result, "check_fail"))
})

test_that("check_schema_df catches multiple invalid columns in a row", {
  skip_if_not_installed("jsonvalidate")
  dat <- data.frame(x = NA, y = "foo")
  schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    },
    "y": {
      "type": "integer"
    }
  },
  "required": ["x", "y"]
}
'
  result <- check_schema_df(dat, schema)
  expect_true(inherits(result, "check_fail"))
  expect_equal(length(result$data[[1]]), 2)
})
