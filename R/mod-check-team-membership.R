#' Check team membership
#'
#' Shiny module to create a modal dialog.
#'
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param team Team ID to check membership in
#' @param user_teams Teams the user is a member of
check_team_membership <- function(input, output, session, team, user_teams) {
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
