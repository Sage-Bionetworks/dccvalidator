server <- function(input, output, session) {

  session$sendCustomMessage(type = "readCookie", message = list())
  setBookmarkExclude(c("cookie", "authorized"))

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(modalDialog(
      title = "Not logged in",
      HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
    ))
  })

  foo <- observeEvent(input$cookie, {

    synLogin(sessionToken = input$cookie)

    # Load data files
    manifest <- reactive({
      validate(need(input$manifest, "Please upload manifest file"))
      read.table(
        input$manifest$datapath,
        sep = "\t",
        header = TRUE,
        na.strings = ""
      )
    })
    indiv <- reactive({
      validate(need(input$indiv_meta, "Enter Synapse ID of individual metadata"))
      indiv <- synGet(input$indiv_meta)
      read.csv(indiv$path)
    })
    biosp <- reactive({
      validate(need(input$biosp_meta, "Enter Synapse ID of biospecimen metadata"))
      biosp <- synGet(input$biosp_meta)
      read.csv(biosp$path)
    })
    assay <- reactive({
      validate(need(input$assay_meta, "Enter Synapse ID of assay metadata"))
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

    ####################
    ####  Metadata  ####
    ####################

    ## Missing columns
    output$missing_cols_indiv <- renderUI({
      report_missing_cols(
        indiv(),
        check_cols_individual,
        "individual",
        input$species
      )
    })

    output$missing_cols_biosp <- renderUI({
      report_missing_cols(
        biosp(),
        check_cols_biospecimen,
        "biospecimen"
      )
    })

    output$missing_cols_assay <- renderUI({
      report_missing_cols(
        assay(),
        check_cols_assay,
        "assay",
        input$assay
      )
    })

    ## Individual IDs
    output$individual_ids <- renderUI({
      validate(
        need(input$indiv_meta, "Enter Synapse ID of individual metadata"),
        need(input$biosp_meta, "Enter Synapse ID of biospecimen metadata")
      )
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
      validate(
        need(input$biosp_meta, "Enter Synapse ID of biospecimen metadata"),
        need(input$assay_meta, "Enter Synapse ID of assay metadata")
      )
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

    output$manifest_cols <- renderUI({
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
      manifest_cols
    })

    output$annot_keys <- renderUI({
      ## Check annotation keys
      annot_keys_results <- check_annotation_keys(manifest())
      annot_keys_results <- setdiff(annot_keys_results, c("path", "parent", "name", "used", "executed")) # remove path, parent
      if (length(annot_keys_results) == 0) {
        annot_keys <- p("Hooray! All annotation keys are valid")
      } else {
        annot_keys <- paste0(
          "The following annotation keys are not part of our annotation dictionary: ",
          paste0(annot_keys_results, collapse = ", ")
        )
      }
      annot_keys
    })

    output$annot_values <- renderUI({
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
      annot_values
    })

    output$specimen_ids_manifest <- renderUI({
      validate(
        need(input$biosp_meta, "Enter Synapse ID of biospecimen metadata"),
        need(input$manifest, "Please upload manifest file")
      )
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
        specimen_ids_manifest_biosp,
        specimen_ids_manifest_assay
      )
    })
  })
}
