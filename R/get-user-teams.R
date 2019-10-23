#' Get teams a user belongs to
#'
#' Looks up the teams a user belongs to. By default, it looks for teams of the
#' current logged in user. You must be logged in to Synapse to use this
#' function.
#'
#' @param user Synapser user object (e.g. output from
#'   [synapser::synGetUserProfile()])
#' @return Character vector of team IDs the user belongs to
#' @examples
#' \dontrun{
#' user <- synapser::synGetUserProfile()
#' get_user_teams(user)
#' }
get_user_teams <- function(user) {
  user_teams <- synapser::synRestGET(
    glue::glue("/user/{user$ownerId}/team?limit=10000")
  )$results

  purrr::map_chr(user_teams, function(x) x$id)
}
