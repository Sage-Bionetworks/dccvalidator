#' Check annotation values
#'
#' Checks that all annotation values are valid. It does not report on values for
#' invalid _keys_; see [check_annotation_keys()].
#'
#' @inheritParams check_annotation_keys
#' @param ... Additional options to [`check_values()`]
#' @return A condition object indicating whether all annotation values are
#'   valid. Invalid annotation values are included as data within the object.
#' @export
#' @seealso [valid_annotation_values()]
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
#'
#' # It is possible to whitelist certain certain values, or all values for
#' # certain keys:
#' check_annotation_values(dat, whitelist_keys = "assay")
#' check_annotation_values(dat, whitelist_values = list(assay = c("foo")))
#' }
check_annotation_values <- function(x, annotations, ...) {
  UseMethod("check_annotation_values", x)
}

#' @export
check_annotation_values.NULL <- function(x, annotations, ...) {
  return(NULL)
}

#' @export
check_annotation_values.File <- function(x, annotations, ...) {
  annots <- synapser::synGetAnnotations(x)
  check_values(
    annots,
    annotations,
    ...,
    return_valid = FALSE
  )
}

#' @export
check_annotation_values.data.frame <- function(x, annotations, ...) {
  check_values(
    x,
    annotations,
    ...,
    return_valid = FALSE
  )
}

#' @export
check_annotation_values.CsvFileTable <- function(x, annotations, ...) {
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
  check_values(
    dat_annots,
    annotations,
    ...,
    return_valid = FALSE
  )
}

#' Valid annotation values
#'
#' Checks for and returns the valid annotation valaues in a data frame, Synapse
#' file, or Synapse file view.
#'
#' @inheritParams check_annotation_values
#' @return A named list of valid annotation values.
#' @export
valid_annotation_values <- function(x, annotations, ...) {
  UseMethod("valid_annotation_values", x)
}

#' @export
valid_annotation_values.NULL <- function(x, annotations, ...) {
  return(NULL)
}

#' @export
valid_annotation_values.File <- function(x, annotations, ...) {
  annots <- synapser::synGetAnnotations(x)
  check_values(
    annots,
    annotations,
    ...,
    return_valid = TRUE
  )
}

#' @export
valid_annotation_values.data.frame <- function(x, annotations, ...) {
  check_values(
    x,
    annotations,
    ...,
    return_valid = TRUE
  )
}

#' @export
valid_annotation_values.CsvFileTable <- function(x, annotations, ...) {
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
  check_values(
    dat_annots,
    annotations,
    ...,
    return_valid = TRUE
  )
}

#' Check that class of value matches annotation columnType
#'
#' @inheritParams check_value
#' @return A vector of invalid values (if `return_valid = FALSE`; otherwise a
#'   vector of valid values).
#' @rdname check_values
check_type <- function(values, key, annotations, whitelist_values = NULL,
                       return_valid = FALSE) {
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
  values <- if (is.factor(values)) as.character(values) else values

  ## Get whitelisted values for key, if any
  whitelist <- unique(whitelist_values[[key]])

  ## Check if class matches
  matches <- class(values) == correct_class
  if (return_valid & matches | !return_valid & !matches) {
    ## Return valid or invalid values, minus whitelisted values
    return(setdiff(unique(stats::na.omit(values)), whitelist))
  } else {
    return(character(0))
  }
}

#' Check values for one key
#'
#' @param values The values of an annotation
#' @param key An annotation key
#' @inheritParams check_values
#' @return A character vector of valid or invalid values
#' @rdname check_values
check_value <- function(values, key, annotations, whitelist_keys = NULL,
                        whitelist_values = NULL, return_valid = FALSE) {
  values <- unlist(values)

  ## Get whitelisted values for key, if any
  whitelist <- unique(whitelist_values[[key]])

  if (missing(annotations)) {
    annotations <- syndccutils::get_synapse_annotations()
  }
  if (!key %in% annotations$key) {
    return(NULL)
  }
  annot_values <- annotations[annotations$key == key, "value"]
  ## If key is being whitelisted, treat all values as valid
  if (key %in% whitelist_keys) {
    if (isTRUE(return_valid)) {
      return(unique(values))
    } else {
      return(character(0))
    }
  }
  ## Some annotation keys don't have enumerated acceptable values (e.g.
  ## specimenID). In that case just check the type.
  if (all(is.na(annot_values))) {
    return(check_type(values, key, annotations, whitelist_values, return_valid))
  }
  ## Check values against enumerated values in annotation definitions.
  if (isTRUE(return_valid)) {
    unique(values[values %in% c(annot_values, whitelist) & !is.na(values)])
  } else {
    unique(values[!values %in% c(annot_values, whitelist) & !is.na(values)])
  }
}

#' Check a set of keys and their values
#'
#' @param x A data frame of annotation data
#' @param annotations A data frame of annotations to check against
#' @param whitelist_keys A character vector of annotation keys to whitelist. If
#'   provided, all values for the given keys will be treated as valid.
#' @param whitelist_values A named list of keys (as the names) and values (as
#'   vectors) to whitelist
#' @param success_msg Message indicating the check succeeded.
#' @param fail_msg Message indicating the check failed.
#' @param return_valid Should the function return valid values? Defaults to
#'   `FALSE` (i.e. the function will return invalid values).
#' @return If `return_valid = FALSE`: a condition object indicating whether all
#'   annotation values are valid. Invalid annotation values are included as data
#'   within the object: a named list where each element corresponds to a key
#'   that contains invalid values, and the contents of each element is a vector
#'   of invalid values. If `return_valid = TRUE`: a named list of the valid
#'   annotation keys and values.
check_values <- function(x, annotations, whitelist_keys = NULL,
                         whitelist_values = NULL,
                         success_msg = "All annotation values are valid",
                         fail_msg = "Some annotation values are invalid",
                         return_valid = FALSE) {
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
  values <- purrr::imap(
    x,
    check_value,
    annotations,
    whitelist_keys = whitelist_keys,
    whitelist_values = whitelist_values,
    return_valid = return_valid
  )
  values <- purrr::compact(values)
  if (isTRUE(return_valid)) {
    return(values)
  }
  if (length(values) == 0) {
    check_pass(
      msg = success_msg,
      behavior = "All annotation values should conform to the vocabulary"
    )
  } else {
    check_fail(
      msg = fail_msg,
      behavior = "All annotation values should conform to the vocabulary",
      data = values
    )
  }
}
