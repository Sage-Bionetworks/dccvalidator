#' Check individual IDs
#'
#' Ensure that all individual IDs in assay metadata are present in individual
#' metadata and vice versa.
#'  
#' @param individual Data frame of individual metadata
#' @param assay Data frame of assay metadata
#' @export
check_indiv_ids <- function(individual, assay) {
  if (!"individualID" %in% colnames(individual) | !"individualID" %in% colnames(assay)) {
    stop(
      "Both individual and assay metadata must contain an `individualID` column",
      call. = FALSE
    )
  }
  
  missing_from_assay      <- setdiff(individual$individualID, assay$individualID)
  missing_from_individual <- setdiff(assay$individualID, individual$individualID)
  
  list(
    missing_from_assay = missing_from_assay,
    missing_from_individual = missing_from_individual
  )
}

