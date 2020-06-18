context("test-check-schema-json.R")

test_that("Valid schemas return check_pass", {
  skip_if_not_installed("jsonvalidate")
  json <- "{}"
  schema <- '{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "properties": {
    "x": {
      "type": "integer"
    }
  }
}
'
  result <- check_schema_json(json, schema)
  expect_true(inherits(result, "check_pass"))
})

test_that("check_schema_json fails for missing property", {
  skip_if_not_installed("jsonvalidate")
  json <- "{}"
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
  result <- check_schema_json(json, schema)
  expect_true(inherits(result, "check_fail"))
})

test_that("check_schema_json fails for wrong type", {
  skip_if_not_installed("jsonvalidate")
  json <- '{
  "x": "foo"
}'
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
  result <- check_schema_json(json, schema)
  expect_true(inherits(result, "check_fail"))
})


test_that("check_schema_json catches multiple errors", {
  skip_if_not_installed("jsonvalidate")
  json <- '{
  "x": "foo",
  "y": "bar"
}'
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
  result <- check_schema_json(json, schema)
  expect_true(inherits(result, "check_fail"))
  expect_equal(length(result$data), 2)
})
