#' Check a data frame of data against a JSON Schema
#'
#' Each row of the data frame will be converted to JSON and validated against
#' the given schema.
#'
#' @param df A data frame whose rows will be converted into JSON and validated
#' @inheritParams check_schema_json
#' @return A condition object indicating whether the data is valid against the
#'   schema.
#' @export
#' @examples
#' if (requireNamespace("jsonvalidate", quietly = TRUE) &
#'       requireNamespace("jsonlite", quietly = TRUE)) {
#' dat <- data.frame(
#'   x = c(NA, 1, NA),
#'   y = c(NA, NA, "foo")
#' )
#' schema <- '{
#'   "$schema": "http://json-schema.org/draft-04/schema#",
#'   "properties": {
#'     "x": {
#'       "type": "integer"
#'     },
#'     "y": {
#'       "type": "integer"
#'     }
#'   },
#'   "required": ["x", "y"]
#' }
#' '
#' check_schema_df(dat, schema)
#' }
check_schema_df <- function(df, schema,
                            success_msg = "Data is valid against the schema",
                            fail_msg = "Data is invalid against the schema") {
  if (!requireNamespace("jsonvalidate", quietly = TRUE)) {
    stop(
      "Package \"jsonvalidate\" needed for this function to work. Please install it.", # nolint
      call. = FALSE
    )
  }
  json_list <- df_to_json_list(df)
  results <- purrr::map(json_list, function(x) {
    jsonvalidate::json_validate(
      json = x,
      schema = schema,
      verbose = TRUE,
      greedy = TRUE,
      engine = "ajv"
    )
  })
  behavior <- "Data should conform to the schema"
  if (all(purrr::map_lgl(results, function(x) x))) {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  } else {
    return_data <- purrr::map(
      results,
      function(x) {
        dat <- attr(x, "errors")
        glue::glue("{dat$dataPath} {dat$message}")
      }
    )
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = return_data
    )
  }
}
