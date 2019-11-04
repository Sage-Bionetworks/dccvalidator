#' App UI
#'
#' Create the UI component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @export
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Metadata Validation"),

    dashboardSidebar(
      sidebarMenu(
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
                    shinydashboard::box(
                      uiOutput("successes"),
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = textOutput("num_success"),
                      status = "success",
                      width = 12
                    ),
                    shinydashboard::box(
                      uiOutput("warnings"),
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = textOutput("num_warn"),
                      status = "warning",
                      width = 12
                    ),
                    shinydashboard::box(
                      uiOutput("failures"),
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      title = textOutput("num_fail"),
                      status = "danger",
                      width = 12
                    )
                  ),
                  tabPanel(
                    "Data summary",
                    fluidRow(
                      shinydashboard::box(
                        title = "Dataset summary",
                        valueBoxOutput("nindividuals"),
                        valueBoxOutput("nspecimens"),
                        valueBoxOutput("ndatafiles"),
                        width = 12
                      )
                    ),
                    fluidRow(
                      shinydashboard::box(
                        title = "File-level summary",
                        selectInput(
                          "file_to_summarize",
                          label = "Choose file to view",
                          choices = c("")
                        ),
                        hr(),
                        plotOutput("datafilevisdat"),
                        verbatimTextOutput("datafileskim"),
                        width = 12
                      )
                    )
                  )
                )
              )
            )
          ),

          # Documentation tab UI
          upload_documents_ui(
            id = "documentation",
            study_link_human = config::get("study_link_human"),
            study_link_animal = config::get("study_link_animal")
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
