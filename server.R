server <- function(input, output, session) {

  session$sendCustomMessage(type = "readCookie", message = list())

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(modalDialog(
      title = "Not logged in",
      HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> and be a member of the AMP-AD Consortium team to use this application. Please log in, and then refresh this page. If you are not a member of the AMP-AD Consortium team, you can request to be added at <a href=\"https://www.synapse.org/#!Team:3320424\"></a>.")
    ))
  })

  foo <- observeEvent(input$cookie, {

    synLogin(sessionToken = input$cookie)

    ## Download annotation definitions
    annots <- get_synapse_annotations()

    ## Create folder for upload
    user <- synGetUserProfile()
    new_folder <- Folder(name = user$get("userName"), parent = "syn20506363")
    created_folder <- synStore(new_folder)

    ## Upload files to Synapse (after renaming them so they keep their original
    ## names)
    observeEvent(input$manifest, {
      save_to_synapse(
        input$manifest,
        parent = created_folder,
        name = input$manifest$name
      )
    })

    observeEvent(input$indiv_meta, {
      save_to_synapse(
        input$indiv_meta,
        parent = created_folder,
        name = input$indiv_meta$name
      )
    })

    observeEvent(input$biosp_meta, {
      save_to_synapse(
        input$biosp_meta,
        parent = created_folder,
        name = input$biosp_meta$name
      )
    })

    observeEvent(input$assay_meta, {
      save_to_synapse(
        input$assay_meta,
        parent = created_folder,
        name = input$assay_meta$name
      )
    })

    ## Load metadata files into session
    manifest <- reactive({
      if (is.null(input$manifest)) return(NULL)
      read.table(
        input$manifest$datapath,
        sep = "\t",
        header = TRUE,
        na.strings = ""
      )
    })
    indiv <- reactive({
      if (is.null(input$indiv_meta)) return(NULL)
      read.csv(input$indiv_meta$datapath, na.strings = "")
    })
    biosp <- reactive({
      if (is.null(input$biosp_meta)) return(NULL)
      read.csv(input$biosp_meta$datapath, na.strings = "")
    })
    assay <- reactive({
      if (is.null(input$assay_meta)) return(NULL)
      read.csv(input$assay_meta$datapath, na.strings = "")
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
      check_indiv_ids_match(indiv(), biosp(), "individual", "biospecimen")
    })
    specimen_ids_biosp_assay <- reactive({
      check_specimen_ids_match(biosp(), assay(), "biospecimen", "assay")
    })
    specimen_ids_biosp_manifest <- reactive({
      check_specimen_ids_match(biosp(), manifest(), "biospecimen", "manifest")
    })
    annotation_keys_manifest <- reactive({
      check_annotation_keys(
        manifest(),
        annots,
        whitelist_keys = c("path", "parent")
      )
    })
    annotation_values_manifest <- reactive({
      check_annotation_values(manifest(), annots)
    })
    annotation_values_indiv <- reactive({
      check_annotation_values(
        indiv(),
        annots,
        whitelist_keys = c("individualID"),
        success_msg = "All values in the individual metadata are valid",
        fail_msg = "Some values in the individual metadata are invalid"
      )
    })
    annotation_values_biosp <- reactive({
      check_annotation_values(
        biosp(),
        annots,
        whitelist_keys = c("specimenID", "individualID"),
        success_msg = "All values in the biospecimen metadata are valid",
        fail_msg = "Some values in the biospecimen metadata are invalid"
      )
    })
    annotation_values_assay <- reactive({
      check_annotation_values(
        assay(),
        annots,
        whitelist_keys = c("specimenID"),
        success_msg = "All values in the assay metadata are valid",
        fail_msg = "Some values in the assay metadata are invalid"
      )
    })
    duplicate_indiv_ids <- reactive({
      check_indiv_ids_dup(indiv())
    })
    duplicate_specimen_ids <- reactive({
      check_specimen_ids_dup(biosp())
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
        annotation_values_assay(),
        duplicate_indiv_ids(),
        duplicate_specimen_ids()
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

    ## Counts of individuals, specimens, and files
    output$nindividuals <- renderValueBox({
      valueBox(
        length(
          unique(
            c(indiv()$individualID, biosp()$individualID)
          )
        ),
        "Individuals",
        icon = icon("users")
      )
    })

    output$nspecimens <- renderValueBox({
      valueBox(
        length(
          unique(
            c(biosp()$specimenID, assay()$specimenID, manifest()$specimenID)
          )
        ),
        "Specimens",
        icon = icon("vial")
      )
    })

    output$ndatafiles <- renderValueBox({
      valueBox(
        length(
          unique(
            manifest()$path
          )
        ),
        "Data files in manifest",
        icon = icon("file")
      )
    })

    ## Placeholder for uploaded data that will be used to determine selectInput
    ## options for the data summary
    inputs <- reactiveVal(c())

    ## Reactive list of data
    vals <- reactive({
      validate(
        need(
          any(
            !is.null(indiv()),
            !is.null(biosp()),
            !is.null(assay()),
            !is.null(manifest())
          ),
          message = "Please upload some data to view a summary"
        )
      )
      list(
        "Individual metadata" = indiv(),
        "Biospecimen metadata" = biosp(),
        "Assay metadata" = assay(),
        "Manifest file" = manifest()
      )
    })

    ## Update selectInput options for which data files to summarize
    observe({
      ## Find which ones are not null
      non_null <- vapply(
        vals(),
        function(x) !is.null(x),
        logical(1)
      )
      inputs(names(which(non_null)))

      updateSelectInput(
        session,
        "file_to_summarize",
        label = "Choose file to view",
        choices = inputs()
      )
    })

    ## skimr summary of data
    output$datafileskim <- renderPrint({
      skim_with(
        numeric = list(
          p0 = NULL,
          p25 = NULL,
          p50 = NULL,
          p75 = NULL,
          p100 = NULL
        ),
        integer = list(
          p0 = NULL,
          p25 = NULL,
          p50 = NULL,
          p75 = NULL,
          p100 = NULL
        )
      )
      ## Validate the data again, otherwise when the user first inputs data an
      ## error will flash briefly
      validate(
        need(
          !is.null((vals()[[input$file_to_summarize]])),
          message = FALSE
        )
      )
      skim(vals()[[input$file_to_summarize]])
    })

    ## visdat summary figure
    output$datafilevisdat <- renderPlot({
      ## Validate the data again, otherwise when the user first inputs data an
      ## error will flash briefly
      validate(
        need(
          !is.null((vals()[[input$file_to_summarize]])),
          message = FALSE
        )
      )
      vis_dat(vals()[[input$file_to_summarize]]) +
        theme(text = element_text(size = 16))
    })
  })
}
