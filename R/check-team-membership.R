#' Check team membership
#'
#' Check if a user is a member of any of the given teams.
#'
#' @keywords internal
#' @param teams Team IDs to check membership in
#' @param user User to check (e.g. output from [synapser::synGetUserProfile()])
#' @inheritParams get_user_teams
check_team_membership <- function(teams, user) {
  user_teams <- get_user_teams(user)
  team_names <- glue::glue_collapse(
    purrr::map_chr(teams, function(x) synapser::synGetTeam(x)$name),
    sep = ", "
  )
  behavior <- glue::glue(
    "You must be a member of one the following Synapse team(s) to use this application: {team_names}." # nolint
  )
  if (!any(teams %in% user_teams)) {
    check_fail(
      msg = "Not a team member",
      behavior = behavior,
      data = setdiff(teams, user_teams)
    )
  } else {
    check_pass(
      msg = "Team membership requirements satisfied",
      behavior = behavior
    )
  }
}
