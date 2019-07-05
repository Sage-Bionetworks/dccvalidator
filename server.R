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
    species_name <- reactive({input$species})
    assay_name <- reactive({input$assay})

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

    ##############################
    ####  Validation Results  ####
    ##############################

    ## Perform checks
    missing_cols_indiv <- reactive({
      check_cols_individual(indiv(), species_name())
    })
    missing_cols_biosp <- reactive({
      check_cols_biospecimen(biosp())
    })
    missing_cols_assay <- reactive({
      check_cols_assay(assay(), assay_name())
    })
    missing_cols_manifest <- reactive({
      check_cols_manifest(manifest())
    })
    individual_ids_indiv_biosp <- reactive({
      check_indiv_ids(indiv(), biosp(), "individual", "biospecimen")
    })
    specimen_ids_biosp_assay <- reactive({
      check_specimen_ids(biosp(), assay(), "biospecimen", "assay")
    })
    specimen_ids_biosp_manifest <- reactive({
      check_specimen_ids(biosp(), manifest(), "biospecimen", "manifest")
    })
    annotation_keys_manifest <- reactive({
      check_annotation_keys(manifest(), whitelist_keys = c("path", "parent"))
    })
    annotation_values_manifest <- reactive({
      check_annotation_values(manifest())
    })

    ## List results
    res <- reactive({
      list(
        missing_cols_indiv(),
        missing_cols_biosp(),
        missing_cols_assay(),
        missing_cols_manifest(),
        individual_ids_indiv_biosp(),
        specimen_ids_biosp_assay(),
        specimen_ids_biosp_manifest(),
        annotation_keys_manifest(),
        annotation_values_manifest()
      )
    })

    ## Successes box
    output$successes <- renderUI({
      successes <- res()[map_lgl(res(), function(x) {inherits(x, "check_pass")})]
      report_results(successes, emoji_prefix = "check")
    })

    ## Warnings box
    output$warnings <- renderUI({
      warnings <- res()[map_lgl(res(), function(x) {inherits(x, "check_warn")})]
      report_results(warnings, emoji_prefix = "warning", verbose = TRUE)
    })

    ## Failures box
    output$failures <- renderUI({
      failures <- res()[map_lgl(res(), function(x) {inherits(x, "check_fail")})]
      report_results(failures, emoji_prefix = "x", verbose = TRUE)
    })
  })
}
