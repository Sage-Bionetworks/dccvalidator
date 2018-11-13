#' Check annotation values
#'
#' Checks that all annotation values. Currently only checks enumerated values
#' for standard annotation keys; it does not check types for annotations such as
#' `compoundDoseUnit` which are required to be strings but have no enumerated
#' list of values.
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
  invalid_values <- purrr::imap(annots, check_values_of_key)
  invalid_values <- purrr::compact(invalid_values)
  report_invalid_values(invalid_values)
  return(invisible(invalid_values))
}

#' @export
check_annotation_values.data.frame <- function(x) {
  invalid_values <- purrr::imap(x, check_values_of_key)
  invalid_values <- purrr::compact(invalid_values)
  report_invalid_values(invalid_values)
  return(invisible(invalid_values))
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
  invalid_values <- purrr::imap(dat_annots, check_values_of_key)
  invalid_values <- purrr::compact(invalid_values)
  report_invalid_values(invalid_values)
  return(invisible(invalid_values))
}

check_values_of_key <- function(value, key) {
  if (!key %in% annotations$key) {
    return(NULL)
  }
  annot_values <- annotations[annotations$key == key, "value"]
  if (all(is.na(annot_values)) | all(value %in% annot_values)) {
    return(NULL)
  } else {
    return(value[!value %in% annot_values])
  }
}

report_invalid_values <- function(invalid_values) {
  if (length(invalid_values) > 0) {
    message("Invalid values found:")
    purrr::iwalk(invalid_values, ~ message(paste0(.y, ": ", paste(.x, sep = ", "))))
  }
}
