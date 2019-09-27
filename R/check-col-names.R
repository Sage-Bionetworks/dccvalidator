#' Check column names against their corresponding template
#'
#' @param data Data frame to check against template (manifest, individual
#'   metadata, or assay metadata)
#' @param template Character vector of column names from the template to check
#'   against
#' @inheritParams check_values
#' @param behavior The intended behavior of the test
#' @return A condition object indicating whether the required columns were
#'   present (`"check_pass"`) or absent (`"check_fail"`).
#' @export
#' @seealso [dccvalidator::get_template()]
check_col_names <- function(data, template, success_msg = NULL, fail_msg = NULL,
                            behavior = NULL) {
  if (is.null(data)) {
    return(NULL)
  }
  missing <- setdiff(template, names(data))
  if (length(missing) > 0) {
    check_fail(msg = fail_msg, behavior = behavior, data = missing)
  } else {
    check_pass(msg = success_msg, behavior = behavior)
  }
}

#' @inheritParams check_col_names
#' @export
#' @rdname check_col_names
#' @examples
#' \dontrun{
#' library("synapser")
#' synLogin()
#' a <- data.frame(path = "/path/file.txt", parent = "syn123", assay = "rnaSeq")
#' check_cols_manifest(a)
#'
#' b <- data.frame(assay = "rnaSeq")
#' check_cols_manifest(b)
#' }
check_cols_manifest <- function(data,
                                success_msg = "All manifest columns present",
                                fail_msg = "Missing columns in the manifest",
                                ...) {
  if (is.null(data)) {
    return(NULL)
  }
  id <- "syn20820080"
  required <- get_template(id, ...)
  behavior <- paste0(
    "Manifest should contain columns: ",
    paste(required, collapse = ", ")
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_individual <- function(data, template,
                                  # nolint start
                                  success_msg = "All individual metadata columns present",
                                  fail_msg = "Missing columns in the individual metadata file",
                                  # nolint end
                                  ...) {
  if (is.null(data)) {
    return(NULL)
  }
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
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_assay <- function(data, template,
                             # nolint start
                             success_msg = "All assay metadata columns present",
                             fail_msg = "Missing columns in the assay metadata file",
                             # nolint end
                             ...) {
  if (is.null(data)) {
    return(NULL)
  }
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
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
}

#' @inheritParams check_col_names
#' @inheritParams get_template
#' @export
#' @rdname check_col_names
check_cols_biospecimen <- function(data, template,
                                   # nolint start
                                   success_msg = "All biospecimen columns present",
                                   fail_msg = "Missing columns in the biospecimen metadata file",
                                   # nolint end
                                   ...) {
  if (is.null(data)) {
    return(NULL)
  }
  template <- match.arg(template, c("general", "drosophila"))
  id <- switch(
    template,
    general = "syn12973252",
    drosophila = "syn20673251"
  )
  required <- get_template(id, ...)
  behavior <- paste0(
    "Biospecimen file should contain columns: ",
    paste(required, collapse = ", ")
  )
  check_col_names(
    data,
    required,
    success_msg = success_msg,
    fail_msg = fail_msg,
    behavior = behavior
  )
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
      "Couldn't download metadata template. Make sure you are logged in to Synapse and that `synID` is a valid synapse ID.", # nolint
      .call = FALSE
    )
  }

  filepath <- template$path
  ext <- tools::file_ext(filepath)

  if (!ext %in% c("xlsx", "csv")) {
    stop(
      "Error loading template: file format must be .csv or .xlsx",
      call. = FALSE
    )
  }

  if (ext == "xlsx") {
    template <- readxl::read_excel(template$path, sheet = 1)
  } else if (ext == "csv") {
    template <- utils::read.csv(template$path, stringsAsFactors = FALSE)
  }
  return(names(template))
}
