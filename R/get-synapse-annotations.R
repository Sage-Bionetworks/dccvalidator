#' Get Synapse annotations
#'
#' Download current annotation values from Synapse and provide them as a data
#' frame.
#'
#' @inheritParams get_synapse_table
#' @return A data frame containing all annotation keys, descriptions, column
#'   types, maximum sizes, values, value descriptions, sources, and the name of
#'   the annotation's parent module.
#' @export
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' get_synapse_annotations(synID = "syn10242922", syn = syn)
#' }
get_synapse_annotations <- function(synID = "syn10242922", syn) {
  get_synapse_table(synID, syn)
}

#' Get Synapse table
#'
#' Get the contents of a Synapse table as a data frame
#'
#' @param synID The Synapse ID of a table to query from. Defaults to
#'   "syn10242922"
#' @param syn Synapse client object
#' @return Data frame of table contents
#' @export
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' get_synapse_table(synID = "syn10242922", syn = syn)
#' }
get_synapse_table <- function(synID, syn) {
  query_result <- syn$tableQuery(
    glue::glue("select * from {synID}"),
    includeRowIdAndRowVersion = FALSE
  )
  dat <- utils::read.csv(
    query_result$filepath,
    na.strings = "",
    stringsAsFactors = FALSE
  )
  dat
}
