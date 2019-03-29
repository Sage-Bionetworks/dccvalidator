server <- function(input, output, session) {

  session$sendCustomMessage(type = "readCookie", message = list())

  foo <- observeEvent(input$cookie, {

    synLogin(sessionToken = input$cookie)

    # Load data files
    manifest <- reactive({
      req(input$manifest)
      read.table(
        input$manifest$datapath,
        sep = "\t",
        header = TRUE,
        na.strings = ""
      )
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

    observeEvent(input$instructions, {
      showModal(
        modalDialog(
          title = "Instructions",
          p("Input the Synapse IDs (e.g. syn12345) for the individual, biospecimen, and assay metadata you have uploaded. The app will check your data for common errors."),
          p("Then, upload your completed manifest. The app will check the metadata annotations in the manifest and ensure that there are no missing specimen IDs between the data files and metadata. To read more about the correct format of a manifest, see this",
            HTML("<a href=\"https://docs.synapse.org/articles/uploading_in_bulk.html\">documentation</a>.")),
          p("Note you must be logged in to Synapse for this application to work."),
          easyClose = TRUE
        )
      )
    })

    observe({
      toggle(
        selector = "span.placeholder",
        condition = any(c(input$indiv_meta, input$biosp_meta, input$assay_meta) == "")
      )
    })

    ####################
    ####  Metadata  ####
    ####################

    ## Missing columns
    output$missing_cols <- renderUI({
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
      missing_cols
    })

    ## Individual IDs
    output$individual_ids <- renderUI({
      ## Check individual IDs between individual and biospecimen files
      individual_ids <- check_indiv_ids(indiv(), biosp()) %>%
        create_mismatched_id_message("individual", "biospecimen", "individual IDs") %>%
        report_mismatched_ids(
          fallback_msg = "Hooray! Individual IDs in the individual and biospecimen files match."
        ) %>%
        ## Look for missing data (NAs) in individualIDs
        add_missing_ids(indiv()$individualID, "individual") %>%
        add_missing_ids(biosp()$individualID, "biospecimen")
      individual_ids
    })

    ## Specimen IDs
    output$specimen_ids <- renderUI({
      ## Check specimen IDs between biospecimen and assay files
      specimen_ids <- check_specimen_ids(biosp(), assay()) %>%
        create_mismatched_id_message("biospecimen", "assay", "specimen IDs") %>%
        report_mismatched_ids(
          fallback_msg = "Hooray! Specimen IDs in the biospecimen and assay files match."
        ) %>%
        ## Look for missing data (NAs) in specimenIDs
        add_missing_ids(biosp()$specimenID, "biospecimen") %>%
        add_missing_ids(assay()$specimenID, "assay")
      specimen_ids
    })

    ########################
    ####  Manifest tab  ####
    ########################

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

      ## Check annotation values and convert list to table for display
      annot_values_results <- check_annotation_values(manifest()) %>%
        create_annotation_value_table()

      if (nrow(annot_values_results) == 0) {
        annot_values <- p("Hooray! All annotation values are valid")
      } else {
        annot_values <- list(
          p("The following annotation values are not part of our annotation dictionary:"),
          datatable(annot_values_results, fillContainer = TRUE)
        )
      }

      ## Check specimen IDs against biospecimen and assay metadata
      specimen_ids_manifest_biosp <- check_specimen_ids(biosp(), manifest()) %>%
        create_mismatched_id_message("biospecimen", "manifest", "specimen IDs") %>%
        report_mismatched_ids(
          fallback_msg = "Hooray! Specimen IDs in the biospecimen and manifest files match."
        )
      specimen_ids_manifest_assay <- check_specimen_ids(assay(), manifest()) %>%
        create_mismatched_id_message("assay", "manifest", "specimen IDs") %>%
        report_mismatched_ids(
          fallback_msg = "Hooray! Specimen IDs in the assay and manifest files match."
        ) %>%
        ## Look for missing data (NAs)
        add_missing_ids(manifest()$specimenID, "manifest")

      list(
        h2("Checking manifest columns"),
        manifest_cols,
        h2("Checking annotations"),
        h3("Checking annotation keys"),
        annot_keys,
        h3("Checking annotation values"),
        annot_values,
        h2("Checking specimen IDs"),
        specimen_ids_manifest_biosp,
        specimen_ids_manifest_assay
      )
    })
  })
}
