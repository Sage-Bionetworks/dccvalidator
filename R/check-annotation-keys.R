#' Check annotation keys
#'
#' Checks that all annotation keys on a file, in a file view, or in a data frame
#' are valid annotations. `check_annotation_keys()` returns any invalid
#' annotation keys; `valid_annotation_keys()` returns _valid_ annotation keys.
#'
#' @param x An object to check.
#' @return A character vector of invalid (for `check_annotation_keys()`) or
#'   valid (for `valid_annotation_keys()`) annotation keys present in `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_keys(my_file)
#'
#' dat <- data.frame(non_annotation = 5, assay = "rnaSeq")
#' check_annotation_keys(dat)
#'
#' fv <- synTableQuery("SELECT * FROM syn17020234")
#' check_annotation_keys(fv)
#' }
check_annotation_keys <- function (x) {
  UseMethod("check_annotation_keys", x)
}

#' @export
check_annotation_keys.File <- function(x) {
  file_annots <- synapser::synGetAnnotations(x)
  check_keys(names(file_annots), return_valid = FALSE)
}

#' @export
check_annotation_keys.data.frame <- function(x) {
  check_keys(names(x), return_valid = FALSE)
}

#' @export
check_annotation_keys.CsvFileTable <- function(x) {
  dat <- synapser::as.data.frame(x)
  fv_synapse_cols <- c(
    "ROW_ID",
    "ROW_VERSION",
    "ROW_ETAG",
    "id",
    "name",
    "createdOn",
    "createdBy",
    "etag",
    "type",
    "currentVersion",
    "parentId",
    "benefactorId",
    "projectId",
    "modifiedOn",
    "modifiedBy",
    "dataFileHandleId"
  )
  dat_annots <- names(dat)[!names(dat) %in% fv_synapse_cols]
  check_keys(dat_annots, return_valid = FALSE)
}

#' @export
#' @rdname check_annotation_keys
valid_annotation_keys <- function(x) {
  UseMethod("valid_annotation_keys", x)
}

#' @export
valid_annotation_keys.File <- function(x) {
  file_annots <- synapser::synGetAnnotations(x)
  check_keys(names(file_annots), return_valid = TRUE)
}

#' @export
valid_annotation_keys.data.frame <- function(x) {
  check_keys(names(x), return_valid = TRUE)
}

#' @export
valid_annotation_keys.CsvFileTable <- function(x) {
  dat <- synapser::as.data.frame(x)
  fv_synapse_cols <- c(
    "ROW_ID",
    "ROW_VERSION",
    "ROW_ETAG",
    "id",
    "name",
    "createdOn",
    "createdBy",
    "etag",
    "type",
    "currentVersion",
    "parentId",
    "benefactorId",
    "projectId",
    "modifiedOn",
    "modifiedBy",
    "dataFileHandleId"
  )
  dat_annots <- names(dat)[!names(dat) %in% fv_synapse_cols]
  check_keys(dat_annots, return_valid = TRUE)
}


check_keys <- function(x, return_valid = FALSE) {
  if (isTRUE(return_valid)) {
    keys <- intersect(x, annotations$key)
    report_keys("Valid keys: ", keys)
  } else {
    keys <- setdiff(x, annotations$key)
    report_keys("Invalid keys: ", keys)
  }
  invisible(keys)
}

report_keys <- function(message, keys) {
  if (length(keys) > 0) {
    message(message)
    message(paste0(keys, collapse = ", "))
  }
}
