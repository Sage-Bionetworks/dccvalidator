#' Check column names against their corresponding template
#'
#' @param data Data file (manifest, individual metadata, or assay metadata)
#' @param template Template to check against
#' @return A character vector containing any column names that are missing from
#'   the data file.
#' @export
check_col_names <- function(data, template) {
  setdiff(template, names(data))
}

#' @export
#' @rdname check_col_names
check_cols_manifest <- function(data) {
  check_col_names(data, manifest_cols())
}

#' @export
#' @rdname check_col_names
check_cols_individual <- function(data, template) {
  template <- match.arg(template, c("human", "animal"))
  check_col_names(data, individual_cols()[[template]])
}

#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, template) {
  template <- match.arg(template, c("rnaSeq", "proteomics"))
  check_col_names(data, assay_cols()[[template]])
}


manifest_cols <- function() {
  c("path", "parent", "name")
}

individual_cols <- function() {
  list(
    human = c(
      "individualID",
      "individualIdSource",
      "species",
      "sex",
      "race",
      "ethnicity",
      "yearsEducation",
      "ageDeath",
      "causeDeath",
      "descriptionDeath",
      "yearAutopsy",
      "apoeGenotype",
      "pmi",
      "pH",
      "brainWeight",
      "diagnosis",
      "diagnosisCriteria"
    ),
    animal = c(
      "individualID",
      "individualIdSource",
      "species",
      "sex",
      "genotype",
      "genotypeBackground",
      "room",
      "litter",
      "matingID",
      "treatmentType"
    )
  )
}


assay_cols <- function() {
  list(
    rnaSeq = c(
      "specimenID",
      "platform",
      "RIN",
      "rnaBatch",
      "libraryBatch",
      "sequencingBatch",
      "libraryPrep",
      "isStranded",
      "runType",
      "readLength"
    ),
    proteomics = c(
      "specimenID",
      "assay",
      "platform",
      "gis",
      "tmTkitBatch",
      "searchEngine",
      "fdr",
      "proteomeDatabase"
    )
  )
}
