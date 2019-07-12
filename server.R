server <- function(input, output, session) {

  session$sendCustomMessage(type = "readCookie", message = list())

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
      validate(need(input$indiv_meta, "Upload individual metadata"))
      indiv <- read.csv(input$indiv_meta$datapath)
    })
    biosp <- reactive({
      validate(need(input$biosp_meta, "Upload biospecimen metadata"))
      biosp <- read.csv(input$biosp_meta$datapath)
    })
    assay <- reactive({
      validate(need(input$assay_meta, "Upload assay metadata"))
      assay <- read.csv(input$assay_meta$datapath)
    })
    species_name <- reactive({input$species})
    assay_name <- reactive({input$assay})

    observeEvent(input$instructions, {
      showModal(
        modalDialog(
          title = "Instructions",
          p("Upload .csv files of your individual, biospecimen, and assay metadata, and upload your manifest as a .tsv or .txt file. The app will check your data for common errors in the metadata and ensure that there are no missing specimen IDs between the metadata and the data files listed in the manifest."),
          p("To read more about the correct format of a manifest, see this",
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
    annotation_values_indiv <- reactive({
      check_annotation_values(
        indiv(),
        whitelist_keys = c("individualID"),
        success_msg = "All values in the individual metadata are valid",
        fail_msg = "Some values in the individual metadata are invalid"
      )
    })
    annotation_values_biosp <- reactive({
      check_annotation_values(
        biosp(),
        whitelist_keys = c("specimenID", "individualID"),
        success_msg = "All values in the biospecimen metadata are valid",
        fail_msg = "Some values in the biospecimen metadata are invalid"
      )
    })
    annotation_values_assay <- reactive({
      check_annotation_values(
        assay(),
        whitelist_keys = c("specimenID"),
        success_msg = "All values in the assay metadata are valid",
        fail_msg = "Some values in the assay metadata are invalid"
      )
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
        annotation_values_manifest(),
        annotation_values_indiv(),
        annotation_values_biosp(),
        annotation_values_assay()
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
