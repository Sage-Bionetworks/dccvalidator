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

    ##########################
    ####  Individual IDs  ####
    ##########################

    ## Check individual IDs between individual and biospecimen files
    individual_ids <- check_indiv_ids(indiv(), biosp()) %>%
      create_mismatched_id_message("individual", "biospecimen", "individual IDs") %>%
      report_mismatched_ids(
        fallback_msg = "Hooray! Individual IDs in the individual and biospecimen files match."
      ) %>%
      ## Look for missing data (NAs) in individualIDs
      add_missing_ids(indiv()$individualID, "individual") %>%
      add_missing_ids(biosp()$individualID, "biospecimen")

    ########################
    ####  Specimen IDs  ####
    ########################

    ## Check specimen IDs between biospecimen and assay files
    specimen_ids <- check_specimen_ids(biosp(), assay()) %>%
      create_mismatched_id_message("biospecimen", "assay", "specimen IDs") %>%
      report_mismatched_ids(
        fallback_msg = "Hooray! Specimen IDs in the biospecimen and assay files match."
      ) %>%
      ## Look for missing data (NAs) in specimenIDs
      add_missing_ids(biosp()$specimenID, "biospecimen") %>%
      add_missing_ids(assay()$specimenID, "assay")

    list(
      h2("Checking column names"),
      missing_cols,
      h2("Checking individual IDs"),
      individual_ids,
      h2("Checking specimen IDs"),
      specimen_ids
    )
  })

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
