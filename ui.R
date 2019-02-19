ui <- fluidPage(

  # Application title
  titlePanel("Metadata Validation"),

  # Sidebar
  sidebarLayout(

    sidebarPanel(

      # Files to be validated
      textInput(
        "indiv_meta",
        "Individual metadata file",
        value = "",
        width = NULL,
        placeholder = "syn123456"
      ),

      radioButtons("species", "Species", c("human", "animal")),

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

      radioButtons("assay_name", "Assay type", c("rnaSeq", "proteomics")),

      fileInput(
        "manifest",
        "Upload Manifest File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),

      # Button to download report
      downloadButton("report", "Generate report")

    ),

    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Metadata", uiOutput("meta_tab")),
        tabPanel("Manifest", verbatimTextOutput("manifest_tab"))
        ## tabPanel("Report", uiOutput("report_tab"))
      )
    )
  )
)
