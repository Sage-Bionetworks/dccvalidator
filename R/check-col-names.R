#' Check column names against their corresponding template
#'
#' @param data Data file (manifest, individual metadata, or assay metadata)
#' @param template Template to check against
#' @return A character vector containing any column names that are missing from
#'   the data file.
#' @export
check_col_names <- function(data, template) {
  template <- match.arg(template, c("manifest", "individual", "assay_rnaseq"))
  template_names <- templist()[[template]]
  return(setdiff(template_names, names(data)))
}


templist <- function() {
  templist <- list(
    manifest = c(
      "path",
      "parent",
      "name",
      "used",
      "executed"
    ),
    individual = c(
      "individualID",
      "individualIdSource",
      "sex",
      "ethnicity",
      "years_of_education",
      "BMI",
      "age_of_death",
      "description_of_death",
      "year_of_autosy",
      "apoe_genotype",
      "PMI",
      "pH",
      "brain_weight"
    ),
    assay_rnaseq = c(
      "individualID",
      "specimenID",
      "file_name",
      "Synapse_ID",
      "species",
      "organ",
      "tissue",
      "cellType",
      "assay",
      "platform",
      "RIN",
      "RNA_isolation_batch",
      "library_batch",
      "sequencing_batch",
      "libraryPrep",
      "isStranded",
      "runType",
      "readLength"
    )
  )
  templist
}
