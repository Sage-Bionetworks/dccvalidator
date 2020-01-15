#' Checks that the name of the study or file is valid
#'
#' Valid strings contain: letters, numbers, spaces, underscores,
#' hyphens, periods, plus signs, and parentheses.
#'
#' @noRd
#' @param name Name of file or study as a string.
#' @return TRUE if name is valid; FALSE otherwise.
#' @examples
#' is_name_valid("foo")
#' is_name_valid("foo?")
is_name_valid <- function(name) {
  valid <- TRUE
  if (is.null(name)) {
    valid <- FALSE
  } else if (name == "") {
    valid <- FALSE
  } else {
    # Check if study name has inappropriate characters
    temp_string <- stringr::str_replace_all(
      name,
      " |\\.|_|-|\\+|\\(|\\)",
      ""
    )
    if (grepl("[[:punct:]]", temp_string)) {
      valid <- FALSE
    }
  }
  valid
}
