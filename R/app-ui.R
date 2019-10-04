#' @import shiny
#' @import shinydashboard

ui_validator <- tabItem(tabName = "validator",
                        # Sidebar
                        sidebarLayout(
                          sidebarPanel(
                            actionButton("instructions", "Show instructions"),
                            br(),
                            br(),

                            # Files to be validated
                            fileInput(
                              "indiv_meta",
                              "Individual metadata file (.csv)",
                              width = NULL,
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                              )
                            ),

                            fileInput(
                              "biosp_meta",
                              "Biospecimen metadata file (.csv)",
                              width = NULL,
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                              )
                            ),

                            fileInput(
                              "assay_meta",
                              "Assay metadata file (.csv)",
                              width = NULL,
                              accept = c(
                                "text/csv",
                                "text/comma-separated-values,text/plain",
                                ".csv"
                              )
                            ),

                            radioButtons("species", "Species", c("animal", "human")),

                            selectInput("assay_name", "Assay type", c("rnaSeq", "proteomics")),

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
                                  title = "Successes",
                                  status = "success",
                                  width = 12
                                ),
                                shinydashboard::box(
                                  uiOutput("warnings"),
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  title = "Warnings",
                                  status = "warning",
                                  width = 12
                                ),
                                shinydashboard::box(
                                  uiOutput("failures"),
                                  solidHeader = TRUE,
                                  collapsible = TRUE,
                                  title = "Failures",
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
)

ui_documentation <- tabItem(tabName = "documentation",
                           # Instructions/Description
                           h3("Upload "),

                           # Ability to choose to add to existing study
                           radioButtons("study_exists",
                                        "Does the study currently exist?",
                                        choices = c("Yes", "No"),
                                        selected = "Yes"),
                           conditionalPanel(
                             condition = "input.study_exists == 'Yes'",
                             selectInput("study_choice",
                                         "Choose the study",
                                         get_studies())
                           ),
                           conditionalPanel(
                             condition = "input.study_exists == 'No'",
                             textInput("study_text",
                                       "Enter the study name")
                           ),

                           # File import
                           fileInput("study_doc",
                                     "Upload the study documentation file"),
                           fileInput("assay_doc",
                                     "Upload the assay documentation files",
                                     multiple = TRUE),

                           with_busy_indicator_ui(
                             actionButton(
                               "upload_docs",
                               "Submit"
                             )
                           )
)

app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Metadata Validation"),

    dashboardSidebar(
      sidebarMenu(
        menuItem("Validator", tabName = "validator"),
        menuItem("Documentation", tabName = "documentation")
      )
    ),

    dashboardBody(

      # Add resources in www
      golem_add_external_resources(),

      tabItems(
        ui_validator,
        ui_documentation
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    includeCSS("inst/app/www/custom.css"),
    singleton(
      includeScript("inst/app/www/readCookie.js")
    )
  )
}
