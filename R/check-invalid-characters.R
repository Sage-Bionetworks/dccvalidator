#' @title Check for non-ascii characters
#'
#' @description Check for non-ascii characters in columns.
#'
#' @param data Data to check
#' @inheritParams check_values
#' @return A condition object indicating whether the data contains columns with
#' a non-ascii character.
#' @export
#' @examples
#' dat <- tibble::tibble(
#'   fails1 = c("study 1", "study&amp;2"),
#'   succeeds = c("file1.ext", "file2.ext"),
#'   fails2 = c("foo<0xa0>", "bar")
#' )
#' check_invalid_characters(dat)
check_invalid_characters <- function(data,
                             success_msg = "There are no invalid characters",
                             fail_msg = "There is an invalid character in a column") { #nolint
  if (is.null(data)) {
    return(NULL)
  }
  has_invalid <- purrr::map_lgl(data, ~ contains_invalid(.))
  behavior <- glue::glue(
    "Only standard ascii characters are allowed."
  )
  if (any(has_invalid)) {
    check_condition(
      msg = fail_msg,
      behavior = behavior,
      data = names(has_invalid)[has_invalid],
      type = "check_fail"
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}

#' Check if a string contains an invalid character
#'
#' @noRd
#' @param text String, or vector of strings, that might have special
#' characters
#' @return `TRUE` if any string contains an invalid character, else `FALSE`
contains_invalid <- function(text) {
  any(purrr::map_lgl(text, function(value) {
    ## Don't flag NA values as unacceptable
    if (is.na(value)) {
      return(FALSE)
    }
    conv <- iconv(value, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    ## Will recieve NA if there's an unacceptable character
    ## Check for other types of invalid patterns
    pattern <- "<0x|&[a-zA-Z0-9]+;|&#[0-9]+;"
    if (is.na(conv) | grepl(pattern, value, useBytes = TRUE)) {
      return(TRUE)
    }
    return(FALSE)
  }))
}

## Summarize all invalid character checks
summarize_invalid_char_check <- function(check_list) {
  ## Only checks that are check_fail
  failed <- purrr::map_lgl(check_list, ~ inherits(., "check_fail"))
  failed_text <- purrr::map_chr(check_list[failed], ~ summarize_check(.))
  glue::glue_collapse(failed_text, sep = "\n")
}

summarize_check <- function(check_result) {
  details <- glue::glue_collapse(check_result$data, sep = ", ")
  glue::glue("Only standard ascii characters are allowed in the files.\n{check_result$message}: {details}")
}
