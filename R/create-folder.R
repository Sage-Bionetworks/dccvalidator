#' Create folder on Synapse
#'
#' Creates and stores a folder on Synapse under the given parent.
#'
#' @noRd
#' @inheritParams get_synapse_annotations
#' @param parent Synapse ID of the parent where the folder should be created
#' @param name Name of the folder to be created
#' @param synapseclient Synapse client module (e.g. output of
#'   `reticulate::import("synapseclient")`)
#' @return Synapse object of the folder that was created
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#'
#' create_folder(
#'   parent = "syn17038062",
#'   name = "my_new_folder",
#'   synapseclient = synapse,
#'   syn = syn
#' )
#' }
create_folder <- function(parent, name, synapseclient, syn) {
  new_folder <- synapseclient$Folder(name = name, parent = parent)
  created_folder <- syn$store(new_folder)
  created_folder
}
