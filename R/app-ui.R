#' @title App UI
#'
#' @description If not running interactively (i.e. locally), launches the
#' \code{\link{mod_synapse_oauth_ui}} to start dccvalidator using OAuth for
#' login. Otherwise, launches the \code{\link{mod_main_ui}} to start
#' dccvalidator using login credentials stored in a .synapseConfig.
#'
#' @export
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request object
#' @return A shinydashboard page
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  div(
    h2("dccvalidator has moved to Synapse"),
    p(
      "You can find the application here: ",
      a("dccvalidator", href = "https://www.synapse.org/#!Wiki:syn25878247/ENTITY")
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "dccvalidator")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/custom.css"),
    tags$script(src = "www/readCookie.js")
  )
}
