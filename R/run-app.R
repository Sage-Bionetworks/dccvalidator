#' Run the Shiny application
#'
#' @param ... Additional golem options passed to [golem::with_golem_options()]
#' @export
#' @return Shiny app with additional golem options passed via `...`
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @examples
#' \dontrun{
#' library("dccvalidator")
#' run_app()
#' }
run_app <- function(...) {
  app <- shinyApp(ui = app_ui_startup_switch, server = app_server)
  with_golem_options(
    app = app,
    golem_opts = list(...)
  )
}
