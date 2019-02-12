#' Extract specimen IDs from a metadata file
#'
#' Given the Synapse ID for a metadata file, this function downloads the file,
#' reads in the first sheet, and looks for the specimenID column. It expects the
#' file to be an .xlsx file (consistent with AMP-AD templates); this may be
#' generalized to accommodate CSV files and other column names in the future.
#'
#' @param fileID Synapse ID of the metadata file to check.
#' @return A character vector of specimen IDs present in the specimenID column
#'   of the file.
#' @export
get_specimenID_metadata <- function(fileID) {
  file <- synapser::synGet(fileID)
  dat <- readxl::read_excel(file$path, sheet = 1)
  if ("specimenID" %in% names(dat)) {
    dat$specimenID
  } else {
    stop("No specimenID column found in metadata", call. = FALSE)
  }
}

#' Compare specimenIDs between data file annotations and metadata file contents
#'
#' @inheritParams get_specimenID_metadata
#' @param synID Synapse IDs of data files annotated with specimenID
#' @return A list of specimen IDs missing from the metadata and from the data.
#' @export
#' @examples
#' \dontrun{
#' synIDs <- c("syn18083968", "syn18083969", "syn18083972", "syn18084124")
#' meta_file <- "syn18084016"
#' find_specimen_mismatches(synIDs, meta_file)
#' }
find_specimen_mismatches <- function(synID, fileID) {
  data_ids <- get_annotation(synID, "specimenID")
  meta_ids <- get_specimenID_metadata(fileID)
  list(
    ## Not using setdiff() for things in missing_from_metadata because I want to
    ## keep the names of the elements.
    missing_from_metadata = data_ids[match(data_ids, meta_ids, 0L) == 0L],
    missing_from_data = setdiff(meta_ids, data_ids)
  )
}
