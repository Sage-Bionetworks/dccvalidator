#' Check column names against their corresponding template
#'
#' @param data Data frame to check against template (manifest, individual
#'   metadata, or assay metadata)
#' @param template Character vector of column names from the template to check
#'   against
#' @inheritParams check_values
#' @param behavior The intended behavior of the test
#' @return A condition object indicating whether the required columns were
#'   present (`"check_pass"`) or absent (`"check_fail"`).
#' @export
#' @seealso [dccvalidator::get_template()]
#' @examples
#' template <- c("individualID", "specimenID", "assay")
#' dat <- data.frame(individualID = c("a", "b"), specimenID = c("a1", "b1"))
#' check_col_names(dat, template)
check_col_names <- function(data, template, success_msg = NULL, fail_msg = NULL,
                            behavior = NULL) {
  if (is.null(data)) {
    return(NULL)
  }
  missing <- setdiff(template, names(data))
  if (length(missing) > 0) {
    check_fail(msg = fail_msg, behavior = behavior, data = missing)
  } else {
    check_pass(msg = success_msg, behavior = behavior)
  }
}

#' @inheritParams check_col_names
#' @param id Synapse ID of the template to check against
#' @export
#' @rdname check_col_names
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#'
#' a <- data.frame(path = "/path/file.txt", parent = "syn123", assay = "rnaSeq")
#' check_cols_manifest(a, syn)
#'
#' b <- data.frame(assay = "rnaSeq")
#' check_cols_manifest(b, syn)
#' }
check_cols_manifest <- function(data, id,
                                success_msg = "All manifest columns present",
                                fail_msg = "Missing columns in the manifest",
                                ...) {
  if (is.null(data)) {
    return(NULL)
  }
  required <- get_template(id = id, ...)
  behavior <- glue::glue(
    "Manifest should contain columns: {glue::glue_collapse(required, sep = ', ')}" # nolint
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_cols_manifest
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_individual <- function(data, id,
                                  # nolint start
                                  success_msg = "All individual metadata column names present",
                                  fail_msg = "Missing columns in the individual metadata file",
                                  # nolint end
                                  ...) {
  if (is.null(data)) {
    return(NULL)
  }
  required <- get_template(id = id, ...)
  behavior <- glue::glue(
    "Individual file should contain columns: {glue::glue_collapse(required, sep = ', ')}. If you do not have data related to a column, the column name should be present and the column should be empty." # nolint
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_cols_manifest
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, id,
                             # nolint start
                             success_msg = "All assay metadata column names present",
                             fail_msg = "Missing columns in the assay metadata file",
                             # nolint end
                             ...) {
  if (is.null(data)) {
    return(NULL)
  }
  required <- get_template(id = id, ...)
  behavior <- glue::glue(
    "Assay file should contain columns: {glue::glue_collapse(required, sep = ', ')}. If you do not have data related to a column, the column name should be present and the column should be empty." # nolint
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_cols_manifest
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_biospecimen <- function(data, id,
                                   # nolint start
                                   success_msg = "All biospecimen column names present",
                                   fail_msg = "Missing columns in the biospecimen metadata file",
                                   # nolint end
                                   ...) {
  if (is.null(data)) {
    return(NULL)
  }
  required <- get_template(id = id, ...)
  behavior <- glue::glue(
    "Biospecimen file should contain columns: {glue::glue_collapse(required, sep = ', ')}. If you do not have data related to a column, the column name should be present and the column should be empty." # nolint
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}
