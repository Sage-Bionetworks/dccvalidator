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
check_pass <- function(msg, behavior, data = NA) {
  rlang::message_cnd(
    "check_pass",
    behavior = behavior,
    data = data,
    message = msg
  )
}

#' @inheritParams check_pass
#' @export
#' @rdname check_pass
check_warn <- function(msg, behavior, data) {
  rlang::warning_cnd(
    "check_warn",
    behavior = behavior,
    data = data,
    message = msg
  )
}

#' @inheritParams check_pass
#' @export
#' @rdname check_pass
check_fail <- function(msg, behavior, data) {
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
check_condtion <- function(msg, behavior, data, type) {
  switch(
    type,
    check_pass = check_pass(msg, behavior, data),
    check_warn = check_pass(msg, behavior, data),
    check_fail = check_pass(msg, behavior, data)
  )
}
