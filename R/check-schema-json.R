#' Check data against a JSON Schema
#'
#' Check a JSON blob against a JSON Schema.
#'
#' @inheritParams check_values
#' @inheritParams jsonvalidate::json_validate
#' @return A condition object indicating whether the data is valid against the
#'   schema.
#' @export
#' @examples
#' if (requireNamespace("jsonvalidate", quietly = TRUE)) {
#' schema <- '{
#'   "$schema": "http://json-schema.org/draft-04/schema#",
#'   "properties": {
#'     "x": {
#'       "type": "integer"
#'     }
#'   },
#'   "required": ["x"]
#' }
#' '
#' json_valid <- '{
#'   "x": 3
#' }'
#' json_invalid <- '{
#'   "x": 1.5
#' }'
#' check_schema_json(json_valid, schema)
#' check_schema_json(json_invalid, schema)
#' }
check_schema_json <- function(json, schema,
                              success_msg = "Data is valid against the schema",
                              fail_msg = "Data is invalid against the schema") {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop(
      "Package \"jsonvalidate\" needed for this function to work. Please install it.", # nolint
      call. = FALSE
    )
  }
  result <- suppressWarnings(
    # (using suppressWarnings to avoid warning about "schema $id ignored")
    jsonvalidate::json_validate(
      json = json,
      schema = schema,
      verbose = TRUE,
      greedy = TRUE,
      engine = "ajv"
    )
  )
  behavior <- "Data should conform to the schema"
  if (result) {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  } else {
    return_data <- attr(result, "errors")
    return_data <- glue::glue("{return_data$dataPath} {return_data$message}")
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = return_data
    )
  }
}
