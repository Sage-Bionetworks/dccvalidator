#' Check for complete columns
#'
#' Check for complete columns in the data and warn (or fail) if present.
#'
#' @param data Data to check
#' @param required_cols The required columns to check for completeness.
#' @param empty_values Values that are considered empty. Defaults to `NA` and
#'   `""`.
#' @param strict If `FALSE`, return a `"check_warn"` object; if `TRUE`, return a
#'   `"check_fail"` object
#' @inheritParams check_values
#' @return A condition object indicating whether the data contains columns that
#'   are not complete.
#' @export
#' @examples
#' dat <- data.frame(specimenID = c("x", "y"), organ = c(NA, NA))
#' check_cols_complete(dat, c("specimenID", "organ"))
check_cols_complete <- function(data, required_cols,
                                empty_values = c(NA, ""), strict = TRUE,
                                success_msg = "Required columns are complete",
                                fail_msg = "Some required columns are not complete") { #nolint
  if (is.null(data)) {
    return(NULL)
  }
  ## Check if all columns have data
  results <- purrr::map_lgl(
    data[, required_cols, drop = FALSE],
    function(x) any(x %in% empty_values)
  )
  behavior <- paste0(
    "Columns ",
    paste(required_cols, collapse = ", "),
    " should be complete.")

  ## Return success if all required columns have complete data.
  ## Otherwise return warn or fail depending on `strict` argument
  if (!any(results)) {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  } else {
    check_condition(
      msg = fail_msg,
      behavior = behavior,
      data = names(which(results)),
      type = ifelse(strict, "check_fail", "check_warn")
    )
  }
}
