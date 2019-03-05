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

    ## Check individual IDs between individual and biospecimen files
    individual_ids <- check_indiv_ids(indiv(), biosp()) %>%
      imap(function(x, name) {
        new_name <- switch(
          name,
          "missing_from_x" = "individual",
          "missing_from_y" = "biospecimen"
        )
        if(length(x) > 0) {
          paste(
            "The following individual IDs are missing from the",
            new_name,
            "metadata file:",
            paste(x, collapse = ", ")
          )
        }
      })
    if (!all(map_lgl(individual_ids, is.null))) {
      individual_ids <- map(individual_ids, tags$p)
    } else {
      individual_ids <- p("Hooray! Individual IDs in the individual and biospecimen files match.")
    }

    ## Check specimen IDs between biospecimen and assay files
    specimen_ids <- check_specimen_ids(biosp(), assay()) %>%
      imap(function(x, name) {
        new_name <- switch(
          name,
          "missing_from_x" = "biospecimen",
          "missing_from_y" = "assay"
        )
        if(length(x) > 0) {
          paste(
            "The following specimen IDs are missing from the",
            new_name,
            "metadata file:",
            paste(x, collapse = ", ")
          )
        }
      })
    if (!all(map_lgl(specimen_ids, is.null))) {
      specimen_ids <- map(specimen_ids, tags$p)
    } else {
      specimen_ids <- p("Hooray! Specimen IDs in the biospecimen and assay files match.")
    }

    list(
      h2("Checking column names"),
      missing_cols,
      h2("Checking individual IDs"),
      individual_ids,
      h2("Checking specimen IDs"),
      specimen_ids
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

  output$manifest_tab <- renderUI({
    ## Check that manifest has path and parent columns
    manifest_cols_results <- check_cols_manifest(manifest())
    if (length(manifest_cols_results) == 0) {
      manifest_cols <- p("Hooray! No columns are missing from metadata.")
    } else {
      manifest_cols <- p(
        paste0(
          "The following column(s) are missing from the manifest: ",
          paste0(manifest_cols_results, collapse = ", ")
        )
      )
    }

    ## Check annotation keys
    annot_keys_results <- check_annotation_keys(manifest())
    annot_keys_results <- setdiff(annot_keys_results, c("path", "parent")) # remove path, parent
    if (length(annot_keys_results) == 0) {
      annot_keys <- p("Hooray! All annotation keys are valid")
    } else {
      annot_keys <- paste0(
        "The following annotation keys are not part of our annotation dictionary: ",
        paste0(annot_keys_results, collapse = ", ")
      )
    }

    list(
      h2("Checking manifest columns"),
      manifest_cols,
      h2("Checking annotations"),
      h3("Checking annotation keys"),
      annot_keys
    )
  })
}
