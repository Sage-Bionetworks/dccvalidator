ui <- function(request) {

  fluidPage(

    includeCSS("www/custom.css"),

    tags$head(
      singleton(
        includeScript("www/readCookie.js")
      )
    ),

    # Application title
    titlePanel("Metadata Validation"),

    # Sidebar
    sidebarLayout(

      sidebarPanel(

        actionButton("instructions", "Show instructions"),
        br(),
        br(),

        # Files to be validated
        textInput(
          "indiv_meta",
          "Individual metadata file",
          value = "",
          width = NULL,
          placeholder = "syn123456"
        ),

        textInput(
          "biosp_meta",
          "Biospecimen metadata file",
          value = "",
          width = NULL,
          placeholder = "syn123456"
        ),

        textInput(
          "assay_meta",
          "Assay metadata file",
          value = "",
          width = NULL,
          placeholder = "syn123456"
        ),

        radioButtons("species", "Species", c("animal", "human")),

        selectInput("assay_name", "Assay type", c("rnaSeq", "proteomics")),

        fileInput(
          "manifest",
          "Upload Manifest File",
          multiple = FALSE,
          accept = c(
            "text/tsv",
            "text/tab-separated-values,text/plain",
            ".tsv"
          )
        ),

        # Bookmark
        bookmarkButton(),

        p("This will bookmark the results from checking metadata files so you can share the results. It will not store results from checking the uploaded manifest file.")

      ),

      # Main panel
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Metadata",
            h2("Checking column names"),
            uiOutput("missing_cols_indiv"),
            uiOutput("missing_cols_biosp"),
            uiOutput("missing_cols_assay"),
            h2("Checking individual IDs"),
            uiOutput("individual_ids"),
            h2("Checking specimen IDs"),
            uiOutput("specimen_ids")
          ),
          tabPanel(
            "Manifest",
            h2("Checking manifest columns"),
            uiOutput("manifest_cols"),
            h2("Checking annotation keys"),
            uiOutput("annot_keys"),
            h2("Checking annotation values"),
            uiOutput("annot_values"),
            h2("Checking specimen IDs"),
            uiOutput("specimen_ids_manifest")
          )
        )
      )
    )
  )
}
