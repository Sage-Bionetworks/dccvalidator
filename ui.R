ui <- fluidPage(

  # Application title
  titlePanel("Metadata Validation"),

  # Sidebar
  sidebarLayout(

    sidebarPanel(

      # Upload files to be validated
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

      fileInput(
        "individual",
        "Upload Individual Metadata File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      radioButtons("species", "Species", c("human", "animal")),

      fileInput(
        "assay",
        "Upload Assay Metadata File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),

      radioButtons("assay_name", "Assay type", c("rnaSeq", "proteomics")),

      # Button to download report
      downloadButton("report", "Generate report")

    ),

    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Manifest", verbatimTextOutput("manifest_tab")),
        tabPanel("Individual Metadata", verbatimTextOutput("indiv_tab")),
        tabPanel("Assay Metadata", verbatimTextOutput("assay_tab"))
      )
    )
  )
)
