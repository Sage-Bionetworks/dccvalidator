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
  check_col_names(data, c("path", "parent"))
}

#' @export
#' @rdname check_col_names
check_cols_individual <- function(data, template) {
  template <- match.arg(template, c("human", "animal"))
  names <- get_template(template)
  check_col_names(data, names)
}

#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, template) {
  template <- match.arg(template, c("rnaSeq", "proteomics"))
  names <- get_template(template)
  check_col_names(data, names)
}

#' @export
#' @rdname check_col_names
check_cols_biospecimen <- function(data) {
  names <- get_template("biospecimen")
  check_col_names(data, names)
}

get_template <- function(name, ...) {
  name <- match.arg(
    name,
    c("human", "animal", "biospecimen", "rnaSeq", "proteomics")
  )
  synID <- switch(
    name,
    human = "syn12973254",
    animal = "syn12973253",
    biospecimen = "syn12973252",
    rnaSeq = "syn12973256",
    proteomics = "syn12973255"
  )
  template <- try(synapser::synGet(synID))
  if (inherits(template, "try-error")) {
    stop(
      "Couldn't download metadata template. Are you logged in to Synapse?",
      .call = FALSE
    )
  } else {
    template <- readxl::read_excel(template$path, sheet = 1)
    return(names(template))
  }
}
