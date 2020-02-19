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

      # Make a list of the tabItems; this is a workaround
      # for a problem with tabItems and shinyDashboard
      tags$div(
        list(
          # Validator tab UI
          tabItem(
            tabName = "validator",

            # Use shinyjs
            shinyjs::useShinyjs(),

            # Sidebar
            sidebarLayout(
              sidebarPanel(
                actionButton("instructions", "Show instructions"),
                br(),
                br(),

                # UI for getting the study name
                get_study_ui("study"),

                shinyjs::disabled(
                  radioButtons(
                    "species",
                    "Species",
                    config::get("species_list")
                  )
                ),

                shinyjs::disabled(
                  selectInput(
                    "assay_name",
                    "Assay type",
                    names(config::get("templates")$assay_templates)
                  )
                ),

                # Files to be validated
                conditionalPanel(
                  condition = "input.species != 'drosophila'",
                  shinyjs::disabled(
                    fileInput(
                      "indiv_meta",
                      "Individual metadata file (.csv)",
                      width = NULL,
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv"
                      )
                    )
                  )
                ),

                shinyjs::disabled(
                  fileInput(
                    "biosp_meta",
                    "Biospecimen metadata file (.csv)",
                    width = NULL,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
                  )
                ),

                shinyjs::disabled(
                  fileInput(
                    "assay_meta",
                    "Assay metadata file (.csv)",
                    width = NULL,
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv"
                    )
                  )
                ),

                shinyjs::disabled(
                  fileInput(
                    "manifest",
                    "Upload Manifest File (.tsv or .txt)",
                    multiple = FALSE,
                    accept = c(
                      "text/tsv",
                      "text/tab-separated-values,text/plain",
                      ".tsv"
                    )
                  )
                ),

                # Add an indicator feature to validate button
                with_busy_indicator_ui(
                  shinyjs::disabled(
                    actionButton(
                      "validate_btn",
                      "Validate"
                    )
                  )
                )
              ),

              # Main panel
              mainPanel(
                tabsetPanel(
                  tabPanel(
                    "Validation Results",
                    br(),
                    results_boxes_ui("Validation Results")
                  ),
                  tabPanel(
                    "Data summary",
                    fluidRow(
                      shinydashboard::box(
                        title = "Dataset summary",
                        valueBoxOutput("nindividuals"),
                        valueBoxOutput("nspecimens"),
                        valueBoxOutput("ndatafiles"),
                        hr(),
                        file_summary_ui("file_summary"),
                        width = 12
                      )
                    )
                  )
                )
              )
            )
          ),
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
