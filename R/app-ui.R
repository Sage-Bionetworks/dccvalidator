#' App UI
#'
#' Create the UI component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @return A shinydashboard page
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Metadata Validation"),

    dashboardSidebar(
      sidebarMenu(
        if (!is.na(config::get("path_to_markdown"))) {
          menuItem("Using the App", tabName = "vignette")
        },
        menuItem("Documentation", tabName = "documentation"),
        menuItem("Validator", tabName = "validator")
      ),
      create_footer(config::get("contact_email"))
    ),

    dashboardBody(

      # Add resources in www
      golem_add_external_resources(),

      # Use shinyjs
      shinyjs::useShinyjs(),

      # Make a list of the tabItems; this is a workaround
      # for a problem with tabItems and shinyDashboard
      tags$div(
        list(
          # Embedd How To Use App vignette
          if (!is.na(config::get("path_to_markdown"))) {
            tabItem(
              tabName = "vignette",
              get_markdown()
            )
          },
          # Documentation tab UI
          upload_documents_ui(
            id = "documentation",
            study_link_human = config::get("study_link_human"),
            study_link_animal = config::get("study_link_animal"),
            study_link_ref = config::get("study_link_ref")
          ),
          # Validator UI
          validator_ui(
            id = "validator",
            species_list = config::get("species_list"),
            assay_templates = config::get("templates")$assay_templates
          )
        ),
        class = "tab-content"
      )
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
