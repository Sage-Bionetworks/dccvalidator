#' Check ids
#'
#' Compare IDs (such as individual IDs or specimen IDs) between two data frames.
#'
#' @param x,y Data frames to compare
#' @param idcol Name of column containing ids to compare
#' @return List of IDs missing from x (but present in y) and missing from y (but
#'   present in x)
#' @export
check_ids <- function(x, y, idcol = c("individualID", "specimenID")) {
  if (!idcol %in% colnames(x) | !idcol %in% colnames(y)) {
    stop(
      paste0("Both x and y must contain column ", idcol),
      call. = FALSE
    )
  }

  ## Ensure that factor columns are coerced to character
  if (is.factor(x[[idcol]])) {
    x[[idcol]] <- as.character(x[[idcol]])
  }
  if (is.factor(y[[idcol]])) {
    y[[idcol]] <- as.character(y[[idcol]])
  }

  missing_from_x <- setdiff(y[[idcol]], x[[idcol]])
  missing_from_y <- setdiff(x[[idcol]], y[[idcol]])

  list(
    missing_from_x = missing_from_x,
    missing_from_y = missing_from_y
  )
}

#' Check individual IDs
#'
#' Ensure that all individual IDs in two data frames match.
#'
#' @inheritParams check_ids
#' @export
#' @rdname check_ids
check_indiv_ids <- function(x, y) {
  check_ids(x, y, "individualID")
}

#' Check specimen IDs
#'
#' Ensure that all specimen IDS in two data frames match
#'
#' @inheritParams check_ids
#' @export
#' @rdname check_ids
check_specimen_ids <- function(x, y) {
  check_ids(x, y, "specimenID")
}
