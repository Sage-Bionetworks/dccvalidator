#' Check uniqueness of individual and specimen IDs
#'
#' @param data Individual metadata file
#' @inheritParams check_values
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
check_indiv_ids_dup <- function(data, success_msg = "Individual IDs are unique",
                                fail_msg = "Duplicate individual IDs found") {
  if (!"individualID" %in% colnames(data)) {
    failure <- check_fail(
      msg = "Can't check for duplicate individual IDs; individualID column missing from data",
      behavior = "Individual metadata should contain individualID column",
      data = colnames(data)
    )
    return(failure)
  }
  behavior <- "Individual IDs within the individual metadata should be unique"
  if (any(duplicated(data$individualID))) {
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = unique(data$individualID[which(duplicated(data$individualID))])
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}

#' @inheritParams check_indiv_ids_dup
#' @rdname check_indiv_ids_dup
#' @export
check_specimen_ids_dup <- function(data, success_msg = "Specimen IDs are unique",
                                   fail_msg = "Duplicate specimen IDs found") {
  if (!"specimenID" %in% colnames(data)) {
    failure <- check_fail(
      msg = "Can't check for duplicate specimen IDs; specimenID column missing from data",
      behavior = "Biospecimen metadata should contain specimenID column",
      data = colnames(data)
    )
    return(failure)
  }
  behavior <- "Specimen IDs within the biospecimen metadata should be unique"
  if (any(duplicated(data$specimenID))) {
    check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = unique(data$specimenID[which(duplicated(data$specimenID))])
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}
