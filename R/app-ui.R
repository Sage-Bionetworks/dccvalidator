#' @import shiny
#' @import shinydashboard
app_ui <- function(request) {
  dashboardPage(
    dashboardHeader(title = "Metadata Validation"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(

      # Add resources in www
      golem_add_external_resources(),

      # Use shinyjs
      shinyjs::useShinyjs(),

      # Sidebar
      sidebarLayout(
        sidebarPanel(
          actionButton("instructions", "Show instructions"),
          br(),
          br(),

          shinyjs::disabled(
            radioButtons(
              "species",
              "Species",
              c("human", "drosophila", "mouse or other animal model" = "general"),
              selected = "general")
          ),

          shinyjs::disabled(
            selectInput("assay_name", "Assay type", c("rnaSeq", "proteomics"))
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
