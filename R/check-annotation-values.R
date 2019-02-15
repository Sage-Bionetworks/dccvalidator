#' Check annotation values
#'
#' Checks that all annotation values are valid. Currently only checks enumerated
#' values for standard annotation keys; it does not check types for annotations
#' such as `compoundDoseUnit` which are required to be strings but have no
#' enumerated list of values. It also does not report on values for invalid
#' _keys_; see [check_annotation_keys()].
#'
#' @inheritParams check_annotation_keys
#' @return A named list of invalid annotation values.
#' @export
#'
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#' annots <- get_synapse_annotations()
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_values(my_file, annots)
#'
#' dat <- data.frame(
#'   non_annotation = 5:7,
#'   assay = c("rnaSeq", "foo", "bar"),
#'   stringsAsFactors = FALSE
#' )
#' check_annotation_values(dat, annots)
#'
#' fv <- synTableQuery("SELECT * FROM syn17020234")
#' check_annotation_values(fv, annots)
#'
#' # If you don't specify an annotations data frame, these functions will
#' # download annotations automatically using `get_synapse_annotations()` (must
#' # be logged in to Synapse)
#' my_file <- synGet("syn11931757", downloadFile = FALSE)
#' check_annotation_values(my_file)
#' }
check_annotation_values <- function (x, annotations) {
  UseMethod("check_annotation_values", x)
}

#' @export
check_annotation_values.File <- function(x, annotations) {
  annots <- synapser::synGetAnnotations(x)
  check_values(annots, annotations, return_valid = FALSE)
}

#' @export
check_annotation_values.data.frame <- function(x, annotations) {
  check_values(x, annotations, return_valid = FALSE)
}

#' @export
check_annotation_values.CsvFileTable <- function(x, annotations) {
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
  check_values(dat_annots, annotations, return_valid = FALSE)
}

#' @export
#' @rdname check_annotation_values
valid_annotation_values <- function (x, annotations) {
  UseMethod("valid_annotation_values", x)
}

#' @export
valid_annotation_values.File <- function(x, annotations) {
  annots <- synapser::synGetAnnotations(x)
  check_values(annots, annotations, return_valid = TRUE)
}

#' @export
valid_annotation_values.data.frame <- function(x, annotations) {
  check_values(x, annotations, return_valid = TRUE)
}

#' @export
valid_annotation_values.CsvFileTable <- function(x, annotations) {
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
  check_values(dat_annots, annotations, return_valid = TRUE)
}

## Check that class of value matches annotation columnType
check_type <- function(value, key, annotations, return_valid = FALSE) {
  coltype <- annotations[annotations$key == key, "columnType"]
  if (inherits(coltype, "tbl_df")) {
    ## need to be sure to get a vector if annotations is a tibble
    coltype <- unlist(coltype)
  }
  correct_class <- switch(
    unique(as.character(coltype)),
    "STRING" = "character",
    "BOOLEAN" = "logical",
    "INTEGER" = "integer",
    "DOUBLE" = "numeric"
  )
  ## Convert factors to strings
  value <- if (is.factor(value)) as.character(value) else value

  ## Check if class matches
  matches <- class(value) == correct_class
  if (isTRUE(return_valid & matches)
      | isFALSE(return_valid) & isFALSE(matches)) {
    return(value)
  } else {
    return(character(0))
  }
}

## Check values for one key
check_value <- function(value, key, annotations, return_valid = FALSE) {
  value <- unlist(value)
  if (missing(annotations)) {
    annotations <- syndccutils::get_synapse_annotations()
  }
  if (!key %in% annotations$key) {
    return(NULL)
  }
  annot_values <- annotations[annotations$key == key, "value"]
  ## Some annotation keys don't have enumerated acceptable values (e.g.
  ## specimenID). In that case just check the type.
  if (all(is.na(annot_values))) {
    return(check_type(value, key, annotations, return_valid))
  }
  ## Check values against enumerated values in annotation definitions.
  if (isTRUE(return_valid)) {
    unique(value[value %in% annot_values & !is.na(value)])
  } else {
    unique(value[!value %in% annot_values & !is.na(value)])
  }
}

## Check a set of keys and their values
check_values <- function(x, annotations, return_valid = FALSE) {
  if (length(names(x)) == 0) {
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
  values <- purrr::imap(x, check_value, annotations, return_valid = return_valid)
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
