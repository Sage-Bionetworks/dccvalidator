#' Check team membership
#'
#' Check if a user is a member of any of the given teams.
#'
#' @export
#' @inheritParams get_synapse_annotations
#' @param teams Team IDs to check membership in
#' @param user User to check (e.g. output from syn$getUserProfile())
#' @return A condition object indicating whether the Synapse user is a member of
#'   the given team(s).
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' user <- syn$getUserProfile("dcctravistest")
#' check_team_membership(teams = "3396691", user = user, syn = syn)
#' check_team_membership(
#'   teams = c("3397398", "3377637"),
#'   user = user,
#'   syn = syn
#' )
#' }
check_team_membership <- function(teams, user, syn) {
  user_teams <- get_user_teams(user, syn = syn)
  team_names <- glue::glue_collapse(
    purrr::map_chr(teams, function(x) syn$getTeam(x)$name),
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
