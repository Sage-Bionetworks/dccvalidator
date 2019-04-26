#' Check column names against their corresponding template
#'
#' @param data Data file (manifest, individual metadata, or assay metadata)
#' @param template Character vector of column names from the template to check
#'   against
#' @return A character vector containing any column names that are missing from
#'   the data file.
#' @export
#' @seealso [dccvalidator::get_template()]
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
  id <- switch(
    template,
    human = "syn12973254",
    animal = "syn12973253"
  )
  names <- get_template(id)
  check_col_names(data, names)
}

#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, template) {
  template <- match.arg(template, c("rnaSeq", "proteomics"))
  id <- switch(
    template,
    rnaSeq = "syn12973256",
    proteomics = "syn12973255"
  )
  names <- get_template(id)
  check_col_names(data, names)
}

#' @export
#' @rdname check_col_names
check_cols_biospecimen <- function(data) {
  names <- get_template("syn12973252")
  check_col_names(data, names)
}

#' Get a template
#'
#' @param synID Synapse ID of an excel or csv file containing a metadata
#'   template
#' @return Character vector of template column names
#' @export
get_template <- function(synID) {
  template <- try(synapser::synGet(synID), silent = TRUE)
  if (inherits(template, "try-error")) {
    stop(
      "Couldn't download metadata template. Make sure you are logged in to Synapse and that `synID` is a valid synapse ID.",
      .call = FALSE
    )
  }

  filepath <- template$path
  ext <- tools::file_ext(filepath)

  if (!ext %in% c("xlsx", "csv")) {
    stop("Error loading template: file format must be .csv or .xlsx", call. = FALSE)
  }

  if (ext == "xlsx") {
    template <- readxl::read_excel(template$path, sheet = 1)
  } else if (ext == "csv") {
    template <- utils::read.csv(template$path, stringsAsFactors = FALSE)
  }
  return(names(template))
}
