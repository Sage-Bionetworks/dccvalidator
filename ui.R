ui <- function(request) {

  fluidPage(

    useShinyjs(),

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

        radioButtons("species", "Species", c("animal", "human"), ),

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

        p("This will bookmark the results from checking metadata stored in Synapse. It will not store results from checking the uploaded manifest file.")

      ),

      # Main panel
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Metadata",
            h2("Column names"),
            p(span(class = "placeholder", "Enter Synapse IDs to check metadata")),
            uiOutput("missing_cols"),
            h2("Individual IDs"),
            p(span(class = "placeholder", "Enter Synapse IDs to check metadata")),
            uiOutput("individual_ids"),
            h2("Specimen IDs"),
            p(span(class = "placeholder", "Enter Synapse IDs to check metadata")),
            uiOutput("specimen_ids")
          ),
          tabPanel("Manifest", uiOutput("manifest_tab"))
        )
      )
    )
  )
}
