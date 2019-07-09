#' Check ids
#'
#' Compare IDs (such as individual IDs or specimen IDs) between two data frames.
#'
#' @param x,y Data frames to compare
#' @param idcol Name of column containing ids to compare
#' @param xname,yname Names of x and y (to be used in resulting messages)
#' @return A condition object indicating whether IDs match (`"check_pass"`) or
#'   not (`"check_fail"`). Mismatched IDs are included as data within the
#'   object.
#' @export
check_ids <- function(x, y, idcol = c("individualID", "specimenID"),
                      xname = NULL, yname = NULL) {
  idcol <- match.arg(idcol)
  if (!idcol %in% colnames(x) | !idcol %in% colnames(y)) {
    failure <- check_fail(
      msg = paste0(
        "Missing column ",
        idcol,
        " in ",
        xname,
        " or ",
        yname,
        " metadata."
      ),
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

  ## Message of correct behavior. Uses the names of the x and y data if present,
  ## otherwise gives a more generic message.
  if (is.null(xname) | is.null(yname)) {
    behavior <- paste0(idcol, " values should match.")
  } else {
    behavior <- paste0(
      idcol,
      " values in the ",
      xname,
      " and ",
      yname,
      " metadata should match."
    )
  }

  ## If nothing is missing, return check_pass
  if (length(missing_from_x) == 0 & length(missing_from_y) == 0) {
    check_pass(
      msg = paste0(
        "All ",
        idcol,
        " values match between ",
        xname,
        " and ",
        yname
      ),
      behavior = behavior
    )
  } else {
    check_fail(
      msg = paste0(
        idcol,
        " values are mismatched between ",
        xname,
        " and ",
        yname
      ),
      behavior = behavior,
      data = list(
        missing_from_x = missing_from_x,
        missing_from_y = missing_from_y
      )
    )
  }
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
