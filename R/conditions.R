#' Create custom conditions for reporting
#'
#' These functions create custom condition objects with subclasses "check_pass",
#' "check_warn", and "check_fail" (inheriting from "message", "warning", or
#' "error", respectively). Validation functions such as
#' [dccvalidator::check_col_names()] use these to report results and provide
#' additional data on the source of errors or invalid data if needed.
#'
#' @param msg Message to report
#' @param behavior Statement of the correct behavior (i.e. what the higher level
#'   function was checking for)
#' @param data Data to return (e.g. invalid values that need attention)
#' @return An S3 object of class "check_pass", "check_warn", or "check_fail"
#' @export
#' @examples
#' check_pass(msg = "Success!", behavior = "Files should be complete")
#' check_warn(
#'   msg = "Warning, some data is missing",
#'   behavior = "Files should be complete",
#'   data = c("specimenID", "assay") # columns with missing data
#' )
#' check_fail(
#'   msg = "Error, some required data is missing",
#'   behavior = "Files should be complete",
#'   data = c("specimenID", "assay") # columns with missing data
#' )
check_pass <- function(msg, behavior, data = NULL) {
  rlang::message_cnd(
    "check_pass",
    behavior = behavior,
    data = data,
    message = msg
  )
}

#' @export
#' @rdname check_pass
check_warn <- function(msg, behavior, data = NULL) {
  rlang::warning_cnd(
    "check_warn",
    behavior = behavior,
    data = data,
    message = msg
  )
}

#' @export
#' @rdname check_pass
check_fail <- function(msg, behavior, data = NULL) {
  rlang::error_cnd(
    "check_fail",
    behavior = behavior,
    data = data,
    message = msg
  )
}

#' Create a condition of the given type
#'
#' @inheritParams check_pass
#' @param type One of "check_pass", "check_warn", "check_fail"
#' @export
#' @inherit check_pass return
#' @examples
#' strict <- TRUE
#' check_condition(
#'   msg = "Some data is missing",
#'   behavior = "Files should be complete",
#'   data = c("specimenID", "assay"),
#'   type = ifelse(strict, "check_fail", "check_warn")
#' )
check_condition <- function(msg, behavior, data, type) {
  switch(
    type,
    check_pass = check_pass(msg, behavior, data),
    check_warn = check_warn(msg, behavior, data),
    check_fail = check_fail(msg, behavior, data)
  )
}
