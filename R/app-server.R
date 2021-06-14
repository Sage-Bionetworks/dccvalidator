#' @title App server
#'
#' @description Create the server-side component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @return none
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_server <- function(input, output, session) {

  ## Synapse client for a specific user
  syn <- synapse$Synapse()

  ## Set client endpoints to staging, if needed
  if (!get_golem_config("production")) {
    set_staging_endpoints(syn)
  }

  if (interactive()) {
    attempt_login(syn)
  } else {
    ## Oauth
    syn <- mod_synapse_oauth_server(
      id = "oauth",
      syn = syn
    )
  }
  shiny::req(inherits(syn, "synapseclient.client.Synapse"), logged_in(syn))
  mod_main_server("main", syn = syn)
}
