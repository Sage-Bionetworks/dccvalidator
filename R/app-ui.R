#' @title App UI
#'
#' @description If not running interactively (i.e. locally), launches the
#' \code{\link{mod_synapse_oauth_ui}} to start dccvalidator using OAuth for
#' login. Otherwise, launches the \code{\link{mod_main_ui}} to start
#' dccvalidator using login credentials stored in a .synapseConfig.
#'
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request object
#' @return A shinydashboard page
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  if (interactive()) {
    ## Running locally; skip OAuth
    mod_main_ui("main")
  } else {
    mod_synapse_oauth_ui(id = "oauth", request = request)
  }
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
