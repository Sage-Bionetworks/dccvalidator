#' Check annotation keys
#'
#' Checks that all annotation keys on a file, in a file view, or in a data frame
#' are valid annotations.
#'
#' @param x An object to check.
#' @return A character vector of invalid annotation keys present in `x`.
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
  invalid_keys <- setdiff(names(file_annots), annotations$key)
  report_invalid_keys(invalid_keys)
  return(invisible(invalid_keys))
}

#' @export
check_annotation_keys.data.frame <- function(x) {
  invalid_keys <- setdiff(names(x), annotations$key)
  report_invalid_keys(invalid_keys)
  return(invisible(invalid_keys))
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
  invalid_keys <- setdiff(names(x), annotations$key)
  report_invalid_keys(invalid_keys)
  return(invisible(invalid_keys))
}

report_invalid_keys <- function(invalid_keys) {
  if (length(invalid_keys) > 0) {
    message("Invalid keys found:")
    message(paste0(invalid_keys, collapse = ", "))
  }
}
