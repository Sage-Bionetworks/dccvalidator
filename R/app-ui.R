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
        if (config::get("docs_tab")$include_tab) {
          menuItem(
            config::get("docs_tab")$tab_name,
            tabName = "documentation") 
        },
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

                div(
                  class = "result",
                  div(
                    class = "wide",
                    shinyjs::disabled(
                      radioButtons(
                        "species",
                        "Species",
                        config::get("species_list")
                      )
                    )
                  ),
                  popify(
                    tags$a(icon(name = "question-circle"), href = "#"),
                    "Information",
                    "Select the species used in the study.",
                    placement = "left",
                    trigger = "hover"
                  )
                ),

                div(
                  class = "result",
                  div(
                    class = "wide",
                    shinyjs::disabled(
                      selectInput(
                        "assay_name",
                        "Assay type",
                        names(config::get("templates")$assay_templates)
                      )
                    )
                  ),
                  popify(
                    tags$a(icon(name = "question-circle"), href = "#"),
                    "Information",
                    "Select the type of assay that matches your assay metadata.", # nolint
                    placement = "left",
                    trigger = "hover"
                  )
                ),

                # Files to be validated
                conditionalPanel(
                  condition = "input.species != 'drosophila'",

                  div(
                    class = "result",
                    div(
                      class = "wide",
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
                    popify(
                      tags$a(icon(name = "question-circle"), href = "#"),
                      "Information",
                      "Select the individual metadata file. This file should have one row per individual, with data about each individual in the experiment. If adding a new dataset to an existing dataset, please include all previous individuals.", # nolint
                      placement = "left",
                      trigger = "hover"
                    )
                  )
                ),

                div(
                  class = "result",
                  div(
                    class = "wide",
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
                    )
                  ),
                  popify(
                    tags$a(icon(name = "question-circle"), href = "#"),
                    "Information",
                    "Select the biospecimen metadata file. This file should have one row per specimen, with data about each specimen in the experiment. If adding a new dataset to an existing dataset, please include all previous specimens.", # nolint
                    placement = "left",
                    trigger = "hover"
                  )
                ),

                div(
                  class = "result",
                  div(
                    class = "wide",
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
                    )
                  ),
                  popify(
                    tags$a(icon(name = "question-circle"), href = "#"),
                    "Information",
                    "Select the assay metadata file. Depending on the assay, this file should have one row per specimen or one row per individual (indicated in the template), with data about the assay performed on each specimen or individual in the experiment. If adding a new dataset to an existing dataset, please include all previous assay specimens or individuals. Please be sure to choose the correct assay type from the drop-down above, as well.", # nolint
                    placement = "left",
                    trigger = "hover"
                  )
                ),

                div(
                  class = "result",
                  div(
                    class = "wide",
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
                    )
                  ),
                  popify(
                    tags$a(icon(name = "question-circle"), href = "#"),
                    "Information",
                    "Select the manifest file. This file should have one row per file to be uploaded to Synapse, including the metadata files, with data about the contents of each file, as well as the study itself. The manifest will be used to upload the data.", # nolint
                    placement = "left",
                    trigger = "hover"
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
                ),
                hr(),
                # Add button to reset the form
                shinyjs::disabled(
                  actionButton(
                    "reset_btn_validate",
                    "Reset"
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
                    "Data Summary",
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
              get_markdown(config::get("path_to_markdown"))
            )
          },
          if (config::get("docs_tab")$include_tab) {
            # Documentation tab UI
            upload_documents_ui(
              id = "documentation",
              markdown_path = config::get("docs_tab")$path_to_docs_markdown,
              include_widget = config::get("docs_tab")$include_upload_widget
            )
          }
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
