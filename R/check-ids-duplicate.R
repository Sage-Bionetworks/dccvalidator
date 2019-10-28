#' Check uniqueness of individual and specimen IDs
#'
#' @param data Individual metadata file
#' @inheritParams check_values
#' @inheritParams check_cols_empty
#' @return A condition object indicating whether the individual IDs in the
#'   individual metadata file are unique.
#' @export
#' @examples
#' dat1 <- data.frame(individualID = c("x", "y", "z", "z"))
#' check_indiv_ids_dup(dat1)
#'
#' dat2 <- data.frame(
#'   individualID = c("x", "y", "z"),
#'   specimenID = c("a", "a", "b")
#' )
#' check_specimen_ids_dup(dat2)
check_indiv_ids_dup <- function(data, empty_values = c(NA, ""),
                                success_msg = "Individual IDs are unique",
                                fail_msg = "Duplicate individual IDs found") {
  if (is.null(data)) {
    return(NULL)
  }
  if (!"individualID" %in% colnames(data)) {
    failure <- check_fail(
      msg = "Can't check for duplicate individual IDs; individualID column missing from data", # nolint
      behavior = "Individual metadata should contain individualID column",
      data = colnames(data)
    )
    return(failure)
  }
  behavior <- "Individual IDs within the individual metadata should be unique"
  results <- purrr::map_lgl(data$individualID, function(x) !x %in% empty_values)
  individualIDs <- data$individualID[results]
  duplicates <- unique(individualIDs[which(duplicated(individualIDs))])
  if (length(duplicates) > 0) {
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = duplicates
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}

#' @inheritParams check_indiv_ids_dup
#' @inheritParams check_cols_empty
#' @rdname check_indiv_ids_dup
#' @export
check_specimen_ids_dup <- function(data, empty_values = c(NA, ""),
                                   success_msg = "Specimen IDs are unique",
                                   fail_msg = "Duplicate specimen IDs found") {
  if (is.null(data)) {
    return(NULL)
  }
  if (!"specimenID" %in% colnames(data)) {
    failure <- check_fail(
      msg = "Can't check for duplicate specimen IDs; specimenID column missing from data", # nolint
      behavior = "Biospecimen metadata should contain specimenID column",
      data = colnames(data)
    )
    return(failure)
  }
  behavior <- "Specimen IDs within the biospecimen metadata should be unique"
  results <- purrr::map_lgl(data$specimenID, function(x) !x %in% empty_values)
  specimenIDs <- data$specimenID[results]
  duplicates <- unique(specimenIDs[which(duplicated(specimenIDs))])
  if (length(duplicates) > 0) {
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = duplicates
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}
