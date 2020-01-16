#' Check annotation keys
#'
#' Checks that all annotation keys on a file, in a file view, or in a data frame
#' are valid annotations. `check_annotation_keys()` returns any invalid
#' annotation keys; `valid_annotation_keys()` returns _valid_ annotation keys.
#'
#' @param x An object to check.
#' @param annotations A data frame of annotation definitions. Must contain at
#'   least three columns: `key`, `value`, and `columnType`.
#' @param ... Additional parameters passed to [`check_keys()`]
#' @param syn Synapse client object
#' @return A condition object indicating whether keys match the given annotation
#'   dictionary. Erroneous keys are included as data within the object.
#' @export
#' @seealso [valid_annotation_keys()]
#'
#' @examples
#' annots <- data.frame(
#'   key = c("assay", "fileFormat", "fileFormat", "fileFormat", "species"),
#'   value = c("rnaSeq", "fastq", "txt", "csv", "Human"),
#'   columnType = c("STRING", "STRING", "STRING", "STRING", "STRING")
#' )
#' dat1 <- data.frame(x = 1)
#' dat2 <- data.frame(assay = "rnaSeq")
#' check_annotation_keys(dat1, annots)
#' check_annotation_keys(dat2, annots)
#'
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' annots <- get_synapse_annotations(syn = syn)
#' my_file <- syn$get("syn11931757", downloadFile = FALSE)
#' check_annotation_keys(my_file, annots, syn)
#'
#' dat <- data.frame(non_annotation = 5, assay = "rnaSeq")
#' check_annotation_keys(dat, annots)
#'
#' fv <- syn$tableQuery("SELECT * FROM syn17020234")
#' check_annotation_keys(fv, annots)
#'
#' # If you don't specify an annotations data frame, these functions will
#' # download annotations automatically using `get_synapse_annotations()` (must
#' # be logged in to Synapse)
#' my_file <- syn$get("syn11931757", downloadFile = FALSE)
#' check_annotation_keys(my_file, syn = syn)
#' }
check_annotation_keys <- function(x, annotations, ...) {
  UseMethod("check_annotation_keys", x)
}

#' @export
#' @describeIn check_annotation_keys Return NULL
check_annotation_keys.NULL <- function(x, annotations, ...) {
  return(NULL)
}

#' @export
#' @describeIn check_annotation_keys Check annotation keys in a Synapse file
check_annotation_keys.synapseclient.entity.File <- function(x, annotations, syn, ...) { # nolint
  file_annots <- dict_to_list(syn$getAnnotations(x))
  check_keys(
    names(file_annots),
    annotations,
    ...,
    return_valid = FALSE
  )
}

#' @export
#' @describeIn check_annotation_keys Check annotation keys in a data frame
check_annotation_keys.data.frame <- function(x, annotations, ...) {
  check_keys(names(x), annotations, ..., return_valid = FALSE)
}

#' @export
#' @describeIn check_annotation_keys Check annotation keys for a Synapse table
check_annotation_keys.synapseclient.table.CsvFileTable <- function(x, annotations, ...) { # nolint
  dat <- utils::read.csv(x$filepath, stringsAsFactors = FALSE)
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
  check_keys(dat_annots, annotations, ..., return_valid = FALSE)
}

#' Valid annotation keys
#'
#' Checks for and returns the valid annotation keys in a data framae, Synapse
#' file, or Synapse file view.
#'
#' @inheritParams check_annotation_keys
#' @param syn Synapse client object
#' @return A vector of valid annotation keys present in `x`.
#' @export
#' @examples
#' annots <- data.frame(
#'   key = c("assay", "fileFormat", "fileFormat", "fileFormat", "species"),
#'   value = c("rnaSeq", "fastq", "txt", "csv", "Human"),
#'   columnType = c("STRING", "STRING", "STRING", "STRING", "STRING")
#' )
#' dat1 <- data.frame(x = 1)
#' dat2 <- data.frame(assay = "rnaSeq")
#' valid_annotation_keys(dat1, annots)
#' valid_annotation_keys(dat2, annots)
valid_annotation_keys <- function(x, annotations, ...) {
  UseMethod("valid_annotation_keys", x)
}

#' @export
#' @describeIn valid_annotation_keys Return NULL
valid_annotation_keys.NULL <- function(x, annotations, ...) {
  return(NULL)
}

#' @export
#' @describeIn valid_annotation_keys Valid annotation keys on a Synapse file
valid_annotation_keys.synapseclient.entity.File <- function(x, annotations, syn, ...) { # nolint
  file_annots <- dict_to_list(syn$getAnnotations(x))
  check_keys(
    names(file_annots),
    annotations,
    ...,
    return_valid = TRUE
  )
}

#' @export
#' @describeIn valid_annotation_keys Valid annotation keys in a data frame
valid_annotation_keys.data.frame <- function(x, annotations, ...) {
  check_keys(names(x), annotations, ..., return_valid = TRUE)
}

#' @export
#' @describeIn valid_annotation_keys Valid annotation keys in a Synapse table
valid_annotation_keys.synapseclient.table.CsvFileTable <- function(x, annotations, ...) { # nolint
  dat <- utils::read.csv(x$filepath, stringsAsFactors = FALSE)
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
  check_keys(dat_annots, annotations, ..., return_valid = TRUE)
}

#' Check that a given set of keys are all present in an annotations dictionary
#'
#' @inheritParams check_values
#' @return A condition object indicating whether keys match the given annotation
#'   dictionary. Erroneous keys are included as data within the object.
#' @export
#' @examples
#' annots <- data.frame(
#'   key = c("fileFormat", "fileFormat"),
#'   value = c("txt", "csv"),
#'   columnType = c("STRING", "STRING"),
#'   stringsAsFactors = FALSE
#' )
#' check_keys("fileFormat", annots)
#' check_keys("x", annots)
check_keys <- function(x, annotations, whitelist_keys = NULL,
                       success_msg = "All annotation keys are valid",
                       fail_msg = "Some annotation keys are invalid",
                       return_valid = FALSE, syn) {
  ## Need to provide data to check
  if (length(x) == 0) {
    stop("No annotations present to check", call. = FALSE)
  }
  ## Get annotations if not passed in
  if (missing(annotations)) {
    annotations <- get_synapse_annotations(syn = syn)
  }
  if (!all(c("key", "value", "columnType") %in% names(annotations))) {
    stop(
      "Annotations must have the following columns: 'key', 'value', and 'columnType'", # nolint
      call. = FALSE
    )
  }
  ## If return_valid is TRUE, just return the valid keys
  if (isTRUE(return_valid)) {
    keys <- intersect(x, c(annotations$key, whitelist_keys))
    return(keys)
  } else {
    ## If return_valid is FALSE, return condition object
    keys <- setdiff(x, annotations$key)
    keys <- setdiff(keys, whitelist_keys)
    behavior <- "All annotation keys should conform to the vocabulary. Refer to the <a target=\"_blank\" href=\"https://shinypro.synapse.org/users/nsanati/annotationUI/\">annotation dictionary</a> for accepted keys." # nolint

    if (length(keys) == 0) {
      check_pass(
        msg = success_msg,
        behavior = behavior
      )
    } else {
      check_fail(
        msg = fail_msg,
        behavior = behavior,
        data = keys
      )
    }
  }
}
