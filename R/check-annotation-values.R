#' Check annotation values
#'
#' Checks that all annotation values are valid. Currently only checks enumerated
#' values for standard annotation keys; it does not check types for annotations
#' such as `compoundDoseUnit` which are required to be strings but have no
#' enumerated list of values. It also does not report on values for invalid
#' _keys_; see [check_annotation_keys()].
#'
#' @param x An object to check.
#' @return A named list of invalid annotation values.
#' @export
#'
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_values(my_file)
#'
#' dat <- data.frame(non_annotation = 5, assay = "rnaSeq")
#' check_annotation_values(dat)
#'
#' fv <- synTableQuery("SELECT * FROM syn17020234")
#' check_annotation_values(fv)
#' }
check_annotation_values <- function (x) {
  UseMethod("check_annotation_values", x)
}

#' @export
check_annotation_values.File <- function(x) {
  annots <- synapser::synGetAnnotations(x)
  check_values(annots, return_valid = FALSE)
}

#' @export
check_annotation_values.data.frame <- function(x) {
  check_values(x, return_valid = FALSE)
}

#' @export
check_annotation_values.CsvFileTable <- function(x) {
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
  dat_annots <- dat[!names(dat) %in% fv_synapse_cols]
  check_values(dat_annots, return_valid = FALSE)
}

#' @export
#' @rdname check_annotation_values
valid_annotation_values <- function (x) {
  UseMethod("valid_annotation_values", x)
}

#' @export
valid_annotation_values.File <- function(x) {
  annots <- synapser::synGetAnnotations(x)
  check_values(annots, return_valid = TRUE)
}

#' @export
valid_annotation_values.data.frame <- function(x) {
  check_values(x, return_valid = TRUE)
}

#' @export
valid_annotation_values.CsvFileTable <- function(x) {
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
  dat_annots <- dat[!names(dat) %in% fv_synapse_cols]
  check_values(dat_annots, return_valid = TRUE)
}

## Check one value against its key
check_value <- function(value, key, return_valid = FALSE) {
  if (!key %in% annotations$key) {
    return(NULL)
  }
  annot_values <- annotations[annotations$key == key, "value"]
  if (isTRUE(return_valid)) {
    unique(value[value %in% annot_values & !is.na(value)])
  } else {
    unique(value[!value %in% annot_values & !is.na(value)])
  }
}

## Check a set of values against their keys
check_values <- function(x, return_valid = FALSE) {
  values <- purrr::imap(x, check_value, return_valid = return_valid)
  values <- purrr::compact(values)

  if (isTRUE(return_valid)) {
    report_values("Valid values: ", values)
  } else {
    report_values("Invalid values: ", values)
  }
  invisible(values)
}

report_values <- function(message, values) {
  if (length(values) > 0) {
    message(message)
    purrr::iwalk(
      values,
      ~ message(paste0(.y, ": ", paste0("\"", .x, "\"", collapse = ", ")))
    )
  }
}
