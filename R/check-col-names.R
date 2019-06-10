#' Check column names against their corresponding template
#'
#' @param data Data file (manifest, individual metadata, or assay metadata)
#' @param template Character vector of column names from the template to check
#'   against
#' @return A condition object indicating whether the required columns were
#'   present (`"check_pass"`) or absent (`"check_fail"`).
#' @export
#' @seealso [dccvalidator::get_template()]
check_col_names <- function(data, template) {
  missing <- setdiff(template, names(data))
}

#' @inheritParams check_col_names
#' @export
#' @rdname check_col_names
check_cols_manifest <- function(data) {
  required <- c("path", "parent")
  behavior <- paste0(
    "Manifest should contain columns: ",
    paste(required, collapse = ", ")
  )
  missing <- check_col_names(data, required)
  if (length(missing) > 0) {
    check_fail(
      msg = "Missing columns in the manifest",
      behavior = behavior,
      data = missing
    )
  } else {
    check_pass(
      msg = "All manifest columns present",
      behavior = behavior
    )
  }
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_individual <- function(data, template, ...) {
  template <- match.arg(template, c("human", "animal"))
  id <- switch(
    template,
    human = "syn12973254",
    animal = "syn12973253"
  )
  required <- get_template(id, ...)
  behavior <- paste0(
    "Individual file should contain columns: ",
    paste(required, collapse = ", ")
  )
  missing <- check_col_names(data, required)
  if (length(missing) > 0) {
    check_fail(
      msg = "Missing columns in the individual metadata file",
      behavior = behavior,
      data = missing
    )
  } else {
    check_pass(
      msg = "All individual metadata columns present",
      behavior = behavior
    )
  }
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, template, ...) {
  template <- match.arg(template, c("rnaSeq", "proteomics"))
  id <- switch(
    template,
    rnaSeq = "syn12973256",
    proteomics = "syn12973255"
  )
  required <- get_template(id, ...)
  behavior <- paste0(
    "Assay file should contain columns: ",
    paste(required, collapse = ", ")
  )
  missing <- check_col_names(data, required)
  if (length(missing) > 0) {
    check_fail(
      msg = "Missing columns in the assay metadata file",
      behavior = behavior,
      data = missing
    )
  } else {
    check_pass(
      msg = "All assay metadata columns present",
      behavior = behavior
    )
  }
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_biospecimen <- function(data, ...) {
  required <- get_template("syn12973252", ...)
  behavior <- paste0(
    "Biospecimen file should contain columns: ",
    paste(required, collapse = ", ")
  )
  missing <- check_col_names(data, required)
  if (length(missing) > 0) {
    check_fail(
      msg = "Missing columns in the biospecimen metadata file",
      behavior = behavior,
      data = missing
    )
  } else {
    check_pass(
      msg = "All biospecimen metadata columns present",
      behavior = behavior
    )
  }
}

#' Get a template
#'
#' @param synID Synapse ID of an excel or csv file containing a metadata
#'   template
#' @param ... Additional arguments passed to [synapser::synGet()]
#' @return Character vector of template column names
#' @export
get_template <- function(synID, ...) {
  template <- try(synapser::synGet(synID, ...), silent = TRUE)
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
