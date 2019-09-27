#' Check team membership
#'
#' Show a modal dialog if a user isn't a member of a given team.
#'
#' @param team Team ID to check membership in
#' @inheritParams get_user_teams
check_team_membership <- function(team, user) {
  user_teams <- get_user_teams(user)
  team_name <- synapser::synGetTeam(team)$name
  if (!team %in% user_teams) {
    showModal(
      modalDialog(
        title = glue::glue("Not in {team_name} team"),
        # nolint start
        HTML(
          glue::glue("You must be a member of the {team_name} team on Synapse to use this tool. If you are not a member of the {team_name} team, you can request to be added at <a href=\"https://www.synapse.org/#!Team:{team}\">https://www.synapse.org/#!Team:{team}</a>.")
        )
        # nolint end
      )
    )
  }
}
