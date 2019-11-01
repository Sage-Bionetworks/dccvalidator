#' Create instructions modal text
#'
#' Provides an overview of the application and how to use it, as well as links
#' to resources about formatting a manifest and the allowable values of
#' annotations.
#'
#' @keywords internal
#' @param annots_link Link to a definition of the annotations being used in the
#'   project
#' @param templates_link Link to the templates used for validation
#' @return A div containin instructions
#' @import shiny
instructions <- function(annots_link, templates_link) {
  # nolint start
  div(
    p(
      "Download relevant",
      HTML(glue::glue("<a target =\"_blank\" href=\"{templates_link}\">metadata templates</a> from the portal."))
    ),
    p("Upload .csv files of your metadata, and upload your manifest as a .tsv or .txt file. The app will check your data for common errors in the metadata and ensure that there are no missing specimen IDs between the metadata and the data files listed in the manifest."),
    p(
      "To read more about the correct format of a manifest, see this",
      HTML("<a target=\"_blank\" href=\"https://docs.synapse.org/articles/uploading_in_bulk.html\">documentation</a>.")
    ),
    p(
      "To explore accepted annotation keys and values, refer to the",
      HTML(glue::glue("<a target=\"_blank\" href=\"{annots_link}\">annotation dictionary</a>."))
    ),
    p("Note you must be logged in to Synapse for this application to work.")
  )
  # nolint end
}
