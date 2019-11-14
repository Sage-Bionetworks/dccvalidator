#' Check for empty columns
#'
#' Check for empty columns in the data and warn (or fail) if present.
#' The function takes in a list of required column names that are
#' not tested for emptiness. This is due to the existing function
#' `check_cols_complete()`, which ensures that the required columns are
#' complete. By ignoring the required columns in `check_cols_empty()`,
#' there are no duplicated results for the same column in the event
#' that a required column was also empty.
#'
#' @param data Data to check
#' @param empty_values Values that are considered empty. Defaults to `NA` and
#'   `""`.
#' @param strict If `FALSE`, return a `"check_warn"` object; if `TRUE`, return a
#'   `"check_fail"` object
#' @inheritParams check_values
#' @inheritParams check_cols_complete
#' @return A condition object indicating whether the data contains columns that
#'   are empty.
#' @export
#' @examples
#' dat <- data.frame(specimenID = c("x", "y"), organ = c(NA, NA))
#' check_cols_empty(dat)
check_cols_empty <- function(data, empty_values = c(NA, ""),
                             required_cols = NULL, strict = FALSE,
                             success_msg = "No columns are empty",
                             fail_msg = "Some columns are empty") {
  if (is.null(data)) {
    return(NULL)
  }
  ## Only check columns that are not required to be complete
  not_required_cols <- setdiff(names(data), required_cols)
  ## Check if all columns have data
  results <- purrr::map_lgl(
    data[, not_required_cols, drop = FALSE],
    function(x) all(x %in% empty_values)
  )
  behavior <- "Completely empty columns might be an accidental omission. If the columns are empty because the data does not exist, then this check can be ignored." # nolint


  ## Return success if all columns have some data. Otherwise return warn or fail
  ## depending on `strict` argument
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
