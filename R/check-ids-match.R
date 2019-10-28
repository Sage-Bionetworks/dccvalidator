#' Check ids
#'
#' Compare IDs (such as individual IDs or specimen IDs) between two data frames.
#'
#' @param x,y Data frames to compare
#' @param idcol Name of column containing ids to compare
#' @param xname,yname Names of x and y (to be used in resulting messages)
#' @param bidirectional Should mismatches from both x and y be reported?
#'   Defaults to `TRUE`; if `FALSE`, will return only IDs in `y` that are not
#'   present in `x` (IDs in `x` but not `y` will be ignored).
#' @return A condition object indicating whether IDs match (`"check_pass"`) or
#'   not (`"check_fail"`). Mismatched IDs are included as data within the
#'   object.
#' @export
check_ids_match <- function(x, y, idcol = c("individualID", "specimenID"),
                            xname = NULL, yname = NULL, bidirectional = TRUE) {
  if (is.null(x) | is.null(y)) {
    return(NULL)
  }
  idcol <- match.arg(idcol)
  if (is.null(xname) | is.null(yname)) {
    ## Give them generic names to be included in the check_fail data if needed
    xname <- xname %||% "x"
    yname <- yname %||% "y"
  }

  if (!idcol %in% colnames(x) | !idcol %in% colnames(y)) {
    failure <- check_fail(
      msg = glue::glue("Missing column {idcol} in {xname} or {yname} metadata"),
      behavior = glue::glue(
        "{xname} and {yname} metadata should both contain a column called {idcol}" # nolint
      ),
      data = stats::setNames(
        list(colnames(x), colnames(y)),
        c(xname, yname)
      )
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

  missing_from_x <- stats::na.omit(setdiff(y[[idcol]], x[[idcol]]))
  missing_from_y <- stats::na.omit(setdiff(x[[idcol]], y[[idcol]]))

  ## Message of correct behavior
  behavior <- glue::glue(
    "{idcol} values in the {xname} and {yname} metadata should match."
  )

  ## If nothing is missing, return check_pass
  if ((length(missing_from_x) == 0 & length(missing_from_y) == 0) |
    (bidirectional == FALSE & length(missing_from_x) == 0)) {
    check_pass(
      msg = glue::glue(
        "All {idcol} values match between {xname} and {yname}"
      ),
      behavior = behavior
    )
  } else {
    check_fail(
      msg = glue::glue(
        "{idcol} values are mismatched between {xname} and {yname}"
      ),
      behavior = behavior,
      data = stats::setNames(
        list(missing_from_x, missing_from_y),
        glue::glue("Missing from {c(xname, yname)}")
      )
    )
  }
}

#' Check individual IDs
#'
#' Ensure that all individual IDs in two data frames match.
#'
#' @inheritParams check_ids_match
#' @export
#' @rdname check_ids_match
#' @examples
#' a <- data.frame(individualID = LETTERS[1:3])
#' b <- data.frame(individualID = LETTERS[1:4])
#' check_specimen_ids_match(a, b, "individual", "biospecimen")
check_indiv_ids_match <- function(x, y, xname = NULL, yname = NULL,
                                  bidirectional = TRUE) {
  check_ids_match(x, y, "individualID", xname, yname, bidirectional)
}

#' Check specimen IDs
#'
#' Ensure that all specimen IDS in two data frames match
#'
#' @inheritParams check_ids_match
#' @export
#' @rdname check_ids_match
#' @examples
#' a <- data.frame(specimenID = LETTERS[1:3])
#' b <- data.frame(specimenID = LETTERS[1:4])
#' check_specimen_ids_match(a, b, "biospecimen", "assay")
check_specimen_ids_match <- function(x, y, xname = NULL, yname = NULL,
                                     bidirectional = TRUE) {
  check_ids_match(x, y, "specimenID", xname, yname, bidirectional)
}
