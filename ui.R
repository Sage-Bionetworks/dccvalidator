ui <- function(request) {
  fluidPage(

    # Application title
    titlePanel("Metadata Validation"),

    # Sidebar
    sidebarLayout(

      sidebarPanel(

        # Files to be validated
        textInput(
          "indiv_meta",
          "Individual metadata file",
          value = "syn17101431",
          width = NULL,
          placeholder = "syn123456"
        ),

        radioButtons("species", "Species", c("human", "animal")),

        textInput(
          "biosp_meta",
          "Biospecimen metadata file",
          value = "syn17101430",
          width = NULL,
          placeholder = "syn123456"
        ),

        textInput(
          "assay_meta",
          "Assay metadata file",
          value = "syn17101433",
          width = NULL,
          placeholder = "syn123456"
        ),

        radioButtons("assay_name", "Assay type", c("rnaSeq", "proteomics")),

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
