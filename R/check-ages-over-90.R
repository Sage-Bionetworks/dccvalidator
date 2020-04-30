#' Check for ages over 90
#'
#' Checks metadata for ages over ninety that should be censored.
#'
#' @param data Data to check
#' @param col Name of age column(s). Defaults to `ageDeath`.
#' @inheritParams check_cols_complete
#' @return A condition object indicating whether the data contains ages over
#'   ninety.
#' @export
#' @examples
#' dat <- data.frame(ageDeath = c(65, 80, 95))
#' check_ages_over_90(dat)
#'
#' # Can check multiple columns
#' dat <- data.frame(age1 = c(50, 55), age2 = c(90, 95))
#' check_ages_over_90(dat)
check_ages_over_90 <- function(data, col = "ageDeath", strict = FALSE,
                               success_msg = "No ages over 90 detected",
                               fail_msg = "Ages over 90 detected in the data") {
  if (is.null(data)) {
    return(NULL)
  }

  behavior <- "Ages over 90 should be censored and written as '90+'."

  ## If ageDeath column isn't present, then return check_pass
  if (!any(col %in% names(data))) {
    return(
      check_pass(
        msg = success_msg,
        behavior = behavior
      )
    )
  }
  cols_in_data <- intersect(col, names(data))
  age_data <- data[, cols_in_data, drop = FALSE]

  if (!any(purrr::map_lgl(age_data, ~ any(is_over_90(.x))))) {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  } else {
    check_condition(
      msg = fail_msg,
      behavior = behavior,
      data = purrr::compact(
        purrr::map(age_data, ~ .x[is_over_90(.x)])
      ),
      type = ifelse(strict, "check_fail", "check_warn")
    )
  }

}

# Does the column (after removing non-numeric characters) contain any values
# >90? NAs are not considered over 90 and will evaluate to FALSE.
is_over_90 <- function(x) {
  if (inherits(x, "character") || inherits(x, "factor")) {
    col_numeric <- as.numeric(gsub("[^0-9|\\.]", "", x))
  } else if (inherits(x, "numeric") || inherits(x, "integer")) {
    col_numeric <- x
  } else {
    col_numeric <- as.numeric(x)
  }
  (col_numeric > 90) %in% TRUE
}
