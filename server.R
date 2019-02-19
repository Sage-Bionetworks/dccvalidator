server <- function(input, output) {

  # Load data files
  manifest <- reactive({
    req(input$manifest)
    read.csv(input$manifest$datapath)
  })
  individual <- reactive({
    req(input$individual)
    read.csv(input$individual$datapath)
  })
  assay <- reactive({
    req(input$assay)
    read.csv(input$assay$datapath)
  })

  # Show data in tabs
  output$manifest_tab <- renderPrint({
    skimr::skim(manifest())
  })

  output$indiv_tab <- renderPrint({
    skimr::skim(individual())
  })

  output$assay_tab <- renderPrint({
    skimr::skim(assay())
  })

  # Generate report
  generate_report <- function(file = "validation_report.html") {
    temp_report <- file.path(tempdir(), "validation_report.Rmd")
    file.copy(
      system.file("rmarkdown/templates/report/skeleton/skeleton.Rmd", package = "dccvalidator"),
      temp_report,
      overwrite = TRUE
    )

    params <- list(
      manifest = manifest(),
      individual = individual(),
      species = input$species,
      assay = assay(),
      assay_name = input$assay_name
    )

    rmarkdown::render(
      temp_report,
      output_file = file,
      params = params,
      envir = new.env(parent = globalenv())
    )
  }

  output$report <- downloadHandler(
    file = "validation_report.html",
    content = function(file) {
      generate_report(file)
    }
  )

  output$report_tab <- renderUI({
    file <- generate_report("validation_report.html")
    includeHTML(file)
  })
}
