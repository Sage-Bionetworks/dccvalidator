#' Check for duplicated file paths
#'
#' The `parent` column in the manifest should not contain duplicated file paths.
#' This function checks if any paths are duplicated.
#'
#' It is possible for this function to return false negatives if the same path
#' is written in different ways. For example, `"~/file.txt"` and
#' `"/Users/me/file.txt"` will be treated as different paths even if they
#' resolve to the same location on the user's machine. Because this function is
#' typically run from a Shiny app without access to the user's filesystem, it is
#' not possible to detect every possible duplicate path.
#'
#' @param data Data to check
#' @inheritParams check_cols_complete
#' @return A condition object indicating whether the data contains duplicated
#'   file paths in the `parent` column.
#' @export
#' @examples
#' manifest <- data.frame(
#'   path = c("/path/to/file.txt", "/path/to/file.txt"),
#'   parent = c("syn123", "syn123")
#' )
#' check_duplicate_paths(manifest)
check_duplicate_paths <- function(data,
                                  success_msg = "No duplicate file paths detected", # nolint
                                  fail_msg = "Duplicate file paths detected") {
  if (is.null(data)) {
    return(NULL)
  }
  behavior <- "File paths in the manifest should be unique; you cannot upload duplicate files." # nolint
  if (!"path" %in% names(data)) {
    return(
      check_warn(
        msg = "Could not check for duplicate file paths because data does not contain `path` column", # nolint
        behavior = behavior,
        data = NULL
      )
    )
  }
  if (all(is.na(data$path))) {
    return(
      check_warn(
        msg = "Could not check for duplicate file paths because all file paths are missing", # nolint
        behavior = behavior,
        data = NULL
      )
    )
  }
  if (anyDuplicated(stats::na.omit(data$path))) {
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = as.character(stats::na.omit(data$path[duplicated(data$path)]))
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}
