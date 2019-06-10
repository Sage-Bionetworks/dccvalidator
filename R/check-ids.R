#' Check ids
#'
#' Compare IDs (such as individual IDs or specimen IDs) between two data frames.
#'
#' @param x,y Data frames to compare
#' @param idcol Name of column containing ids to compare
#' @param xname,yname Names of x and y (to be used in resulting messages)
#' @return List of IDs missing from x (but present in y) and missing from y (but
#'   present in x)
#' @export
check_ids <- function(x, y, idcol = c("individualID", "specimenID"),
                      xname = NULL, yname = NULL) {
  idcol <- match.arg(idcol)
  if (!idcol %in% colnames(x) | !idcol %in% colnames(y)) {
    failure <- check_fail(
      msg = paste0("Missing column ", idcol, " in ", xname, " or ", yname),
      behavior = paste0(
        xname,
        " and ",
        yname,
        " metadata should both contain a column called ",
        idcol
      ),
      data = list(colnames(x), colnames(y))
    )
    return(failure)
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
check_indiv_ids <- function(x, y, xname = NULL, yname = NULL) {
  check_ids(x, y, "individualID", xname, yname)
}

#' Check specimen IDs
#'
#' Ensure that all specimen IDS in two data frames match
#'
#' @inheritParams check_ids
#' @export
#' @rdname check_ids
check_specimen_ids <- function(x, y, xname = NULL, yname = NULL) {
  check_ids(x, y, "specimenID", xname, yname)
}
