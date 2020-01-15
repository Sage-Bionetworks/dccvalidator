#' Get teams a user belongs to
#'
#' Looks up the teams a user belongs to. By default, it looks for teams of the
#' current logged in user. You must be logged in to Synapse to use this
#' function.
#'
#' @noRd
#' @inheritParams get_synapse_annotations
#' @param user Synapse user object (e.g. output from syn$getUserProfile())
#' @return Character vector of team IDs the user belongs to
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' user <- syn$getUserProfile()
#' get_user_teams(user, syn)
#' }
get_user_teams <- function(user, syn) {
  user_teams <- syn$restGET(
    glue::glue("/user/{user$ownerId}/team?limit=10000")
  )$results

  purrr::map_chr(user_teams, function(x) x$id)
}
