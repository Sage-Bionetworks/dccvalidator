#' Check annotation keys
#'
#' Checks that all annotation keys on a file, in a file view, or in a data frame
#' are valid annotations. `check_annotation_keys()` returns any invalid
#' annotation keys; `valid_annotation_keys()` returns _valid_ annotation keys.
#'
#' @param x An object to check.
#' @param annotations A data frame of annotation definitions. Must contain at
#'   least three columns: `key`, `value`, and `columnType`.
#' @return A character vector of invalid (for `check_annotation_keys()`) or
#'   valid (for `valid_annotation_keys()`) annotation keys present in `x`.
#' @export
#'
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#' annots <- get_synapse_annotations()
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_keys(my_file, annots)
#'
#' dat <- data.frame(non_annotation = 5, assay = "rnaSeq")
#' check_annotation_keys(dat, annots)
#'
#' fv <- synTableQuery("SELECT * FROM syn17020234")
#' check_annotation_keys(fv, annots)
#'
#' # If you don't specify an annotations data frame, these functions will
#' # download annotations automatically using `get_synapse_annotations()` (must
#' # be logged in to Synapse)
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_keys(my_file)
#' }
check_annotation_keys <- function (x, annotations) {
  UseMethod("check_annotation_keys", x)
}

#' @export
check_annotation_keys.File <- function(x, annotations) {
  file_annots <- synapser::synGetAnnotations(x)
  check_keys(names(file_annots), annotations, return_valid = FALSE)
}

#' @export
check_annotation_keys.data.frame <- function(x, annotations) {
  check_keys(names(x), annotations, return_valid = FALSE)
}

#' @export
check_annotation_keys.CsvFileTable <- function(x, annotations) {
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
  check_keys(dat_annots, annotations, return_valid = FALSE)
}

#' @export
#' @rdname check_annotation_keys
valid_annotation_keys <- function(x, annotations) {
  UseMethod("valid_annotation_keys", x)
}

#' @export
valid_annotation_keys.File <- function(x, annotations) {
  file_annots <- synapser::synGetAnnotations(x)
  check_keys(names(file_annots), annotations, return_valid = TRUE)
}

#' @export
valid_annotation_keys.data.frame <- function(x, annotations) {
  check_keys(names(x), annotations, return_valid = TRUE)
}

#' @export
valid_annotation_keys.CsvFileTable <- function(x, annotations) {
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
  check_keys(dat_annots, annotations, return_valid = TRUE)
}


check_keys <- function(x, annotations, return_valid = FALSE) {
  if (length(x) == 0) {
    stop("No annotations present to check", call. = FALSE)
  }
  if (missing(annotations)) {
    annotations <- syndccutils::get_synapse_annotations()
  }
  if (!all(c("key", "value", "columnType") %in% names(annotations))) {
    stop(
      "Annotations must have the following columns: 'key', 'value', and 'columnType'",
      call. = FALSE
    )
  }
  if (isTRUE(return_valid)) {
    keys <- intersect(x, annotations$key)
  } else {
    keys <- setdiff(x, annotations$key)
  }
  keys
}
