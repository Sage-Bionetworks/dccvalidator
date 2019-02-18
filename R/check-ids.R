#' Check individual IDs
#'
#' Ensure that all individual IDs in two data frames match.
#'  
#' @param x,y Data frames of metadata to compare
#' @return A list of individual IDs missing from x (but present in y) and
#'   missing from y (but present in x)
#' @export
check_indiv_ids <- function(x, y) {
  if (!"individualID" %in% colnames(x) | !"individualID" %in% colnames(y)) {
    stop(
      "Both x and y must contain an `individualID` column",
      call. = FALSE
    )
  }

  ## Ensure that factor columns are coerced to character
  if (is.factor(x$individualID)) {
    x$individualID <- as.character(x$individualID)
  }
  if (is.factor(y$individualID)) {
    y$individualID <- as.character(y$individualID)
  }

  missing_from_x <- setdiff(y$individualID, x$individualID)
  missing_from_y <- setdiff(x$individualID, y$individualID)
  
  list(
    missing_from_x = missing_from_x,
    missing_from_y = missing_from_y
  )
}

