server <- function(input, output) {

  synLogin()

  # Load data files
  manifest <- reactive({
    req(input$manifest)
    read.table(input$manifest$datapath, sep = "\t", header = TRUE)
  })
  indiv <- reactive({
    req(input$indiv_meta)
    indiv <- synGet(input$indiv_meta)
    read.csv(indiv$path)
  })
  biosp <- reactive({
    req(input$biosp_meta)
    biosp <- synGet(input$biosp_meta)
    read.csv(biosp$path)
  })
  assay <- reactive({
    req(input$assay_meta)
    assay <- synGet(input$assay_meta)
    read.csv(assay$path)
  })

  # Show data in tabs
  output$meta_tab <- renderUI({

    ## Check column names
    missing_cols <- list(
      individual = check_cols_individual(indiv(), input$species),
      biospecimen = check_cols_biospecimen(biosp()),
      assay = check_cols_assay(assay(), input$assay)
    ) %>%
      imap(function (x, name) {
        if (length(x) > 0) {
          paste0(
            "Columns missing in ",
            name,
            " metadata template: ",
            paste(x, collapse = ", ")
          )
        }
      })

    if (!all(map_lgl(missing_cols, is.null))) {
      missing_cols <- map(missing_cols, tags$p)
    } else {
      missing_cols <- p("Hooray! No columns are missing from metadata.")
    }

    list(
      h2("Checking column names"),
      missing_cols
    )
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
      individual = indiv(),
      indiv_template = input$species,
      biospecimen = biosp(),
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
