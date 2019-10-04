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
                           h3("Upload Unstructured Metadata"),
                           #nolint start
                           p("Unstructured metadata is similar to the materials and methods in a paper. These are used in the portal to give a summary of the project, assays, and other relevant information. An example of what this information should include and how it will appear in the portal can be found ", tags$a(href = "https://adknowledgeportal.synapse.org/#/Explore/Studies?Study=syn8391648", "here"), "."),
                           
                           h4("Study Description"),
                           
                           p("The study description is an overview of the study and should include:"),
                           
                           tags$ul(
                             tags$li("human studies", tags$ul(tags$li("how the data was obtained, as well as a summary description of the data, including study type (prospective cohort, case-control, or post-mortem), disease focus, inclusion/exclusion criteria, and number of participants or donors. For post mortem studies, include the brain bank name(s) and tissue(s) that were sampled."))),
                             tags$li("model studies", tags$ul(tags$li("where the models were generated, as well as a summary description of the model, including common name, genetic background, and a link to the strain datasheet, or datasheets if a cross between two strains.")))
                           ),
                           
                           h4("Assay Description"),
                           
                           p("The assay description should include a summary of sample processing, data generation, and data processing."),
                           
                           #nolint end
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
                                     "Upload the study description file"),
                           fileInput("assay_doc",
                                     "Upload the assay description files",
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
