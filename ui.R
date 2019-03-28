ui <- function(request) {

  fluidPage(

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
          tabPanel("Metadata", uiOutput("meta_tab")),
          tabPanel("Manifest", uiOutput("manifest_tab"))
        )
      )
    )
  )
}
