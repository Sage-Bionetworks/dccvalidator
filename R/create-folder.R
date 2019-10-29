#' Create folder on Synapse
#'
#' Creates and stores a folder on Synapse under the given parent.
#'
#' @keywords internal
#' @param parent Synapse ID of the parent where the folder should be created
#' @param name Name of the folder to be created
#' @return Synapse object of the folder that was created
#' @examples
#' \dontrun{
#' create_folder(parent = "syn17038062", name = "my_new_folder")
#' }
create_folder <- function(parent, name) {
  new_folder <- synapser::Folder(name = name, parent = parent)
  created_folder <- synapser::synStore(new_folder)
  created_folder
}
