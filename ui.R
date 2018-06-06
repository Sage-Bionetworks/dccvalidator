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

      # Button to download report
      downloadButton("report", "Generate report")

    ),

    # Main panel
    mainPanel(
      tabsetPanel(
        tabPanel("Manifest", tableOutput("manifest_tab")),
        tabPanel("Individual Metadata", tableOutput("indiv_tab")),
        tabPanel("Assay Metadata", tableOutput("assay_tab"))
      )
    )
  )
)
