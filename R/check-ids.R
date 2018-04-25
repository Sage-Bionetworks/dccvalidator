#' Check individual IDs
#'
#' Ensure that all individual IDs in assay metadata are present in clinical
#' metadata and vice versa.
#'  
#' @param clinical Data frame of clinical metadata
#' @param assay Data frame of assay metadata
#' @export
check_indiv_ids <- function(clinical, assay) {
  if (!"individualID" %in% colnames(clinical) | !"individualID" %in% colnames(assay)) {
    stop(
      "Both clinical and assay metadata must contain an `individualID` column",
      call. = FALSE
    )
  }
  
  missing_from_assay    <- setdiff(clinical$indID, assay$indID)
  missing_from_clinical <- setdiff(assay$indID, clinical$indID)
  
  list(
    missing_from_assay = missing_from_assay,
    missing_from_clinical = missing_from_clinical
  )
}

