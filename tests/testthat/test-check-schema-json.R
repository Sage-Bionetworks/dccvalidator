context("test-check-schema-json.R")

test_that("Valid schemas return check_pass", {
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

test_that("check_schema_json works with references", {
  parent <- c(
    "{",
    '  "$schema": "http://json-schema.org/draft-07/schema#",',
    '  "type": "object",',
    '  "properties": {',
    '    "x": {',
    '      "$ref": "child.json"',
    "    }",
    "  },",
    '  "required": ["x"]',
    "}"
  )
  child <- c(
    "{",
    '    "type": "integer"',
    "}"
  )
  path <- tempfile()
  dir.create(path)
  writeLines(parent, file.path(path, "parent.json"))
  writeLines(child, file.path(path, "child.json"))

  valid <- '{"x": 1}'
  invalid <- '{"x": "foo"}'

  res1 <- check_schema_json(
    json = valid,
    schema = file.path(path, "parent.json")
  )
  res2 <- check_schema_json(
    json = invalid,
    schema = file.path(path, "parent.json")
  )
  expect_true(inherits(res1, "check_pass"))
  expect_true(inherits(res2, "check_fail"))
})
