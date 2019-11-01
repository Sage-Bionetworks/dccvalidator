#' App server
#'
#' Create the server-side component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @export
app_server <- function(input, output, session) {
  ## Initial titles for report boxes
  reporting_titles <- reactiveValues(
    success = "Successes (0)",
    warn = "Warnings (0)",
    fail = "Failures (0)"
  )
  output$num_success <- renderText(reporting_titles$success)
  output$num_warn <- renderText(reporting_titles$warn)
  output$num_fail <- renderText(reporting_titles$fail)

  session$sendCustomMessage(type = "readCookie", message = list())

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a target=\"_blank\" href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.") # nolint
      )
    )
  })

  foo <- observeEvent(input$cookie, {
    synapser::synLogin(sessionToken = input$cookie)

    ## Check if user is in AMP-AD Consortium team (needed in order to create
    ## folder at the next step), and if they are a certified user.
    user <- synapser::synGetUserProfile()
    membership <- check_team_membership(
      teams = config::get("teams"),
      user = user
    )
    certified <- check_certified_user(user$ownerId)
    report_unsatisfied_requirements(membership, certified)

    ## If user is a member of the team(s), create folder to save files and
    ## enable inputs
    if (inherits(membership, "check_pass") &
      inherits(certified, "check_pass")) {
      created_folder <- try(
        create_folder(
          parent = config::get("parent"),
          name = user$userName
        )
      )

      study_name <- callModule(
        get_study_server,
        "study",
        study_table_id = reactive(config::get("study_table"))
      )

      inputs_to_enable <- c(
        "indiv_meta",
        "biosp_meta",
        "assay_meta",
        "manifest",
        "species",
        "assay_name",
        "validate_btn"
      )
      purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

      # Documentation server needs created_folder to run correctly
      callModule(
        upload_documents_server,
        "documentation",
        parent_folder = reactive(created_folder),
        study_table_id = reactive(config::get("study_table"))
      )
    }

    ## If drosophila species checked, reset fileInput
    observeEvent(input$species, {
      if (input$species == "drosophila") {
        shinyjs::reset("indiv_meta")
        files$indiv <- NULL
      }
    })

    ## Download annotation definitions
    annots <- get_synapse_annotations(synID = config::get("annotations_table"))

    ## Store files in separate variable to be able to reset inputs to NULL
    files <- reactiveValues(
      indiv = NULL,
      manifest = NULL,
      biosp = NULL,
      assay = NULL
    )
    ## Upload files to Synapse (after renaming them so they keep their original
    ## names)
    observeEvent(input$manifest, {
      files$manifest <- input$manifest
    })

    observeEvent(input$indiv_meta, {
      files$indiv <- input$indiv_meta
    })

    observeEvent(input$biosp_meta, {
      files$biosp <- input$biosp_meta
    })

    observeEvent(input$assay_meta, {
      files$assay <- input$assay_meta
    })

    ## Load metadata files into session
    manifest <- reactive({
      if (is.null(files$manifest)) {
        return(NULL)
      }
      utils::read.table(
        files$manifest$datapath,
        sep = "\t",
        header = TRUE,
        na.strings = "",
        stringsAsFactors = FALSE
      )
    })
    indiv <- reactive({
      if (is.null(files$indiv)) {
        return(NULL)
      }
      utils::read.csv(
        files$indiv$datapath,
        na.strings = "",
        stringsAsFactors = FALSE
      )
    })
    biosp <- reactive({
      if (is.null(files$biosp)) {
        return(NULL)
      }
      utils::read.csv(
        files$biosp$datapath,
        na.strings = "",
        stringsAsFactors = FALSE
      )
    })
    assay <- reactive({
      if (is.null(files$assay)) {
        return(NULL)
      }
      utils::read.csv(
        files$assay$datapath,
        na.strings = "",
        stringsAsFactors = FALSE
      )
    })

    # Location of templates
    indiv_template <- reactive({
      if (input$species == "human") {
        config::get("templates")$individual_templates[["human"]]
      } else {
        config::get("templates")$individual_templates[["animal"]]
      }
    })
    biosp_template <- reactive({
      if (input$species == "drosophila") {
        config::get("templates")$biospecimen_templates[["drosophila"]]
      } else {
        config::get("templates")$biospecimen_templates[["general"]]
      }
    })
    assay_template <- reactive({
      config::get("templates")$assay_templates[[input$assay_name]]
    })
    species_name <- reactive({
      input$species
    })
    assay_name <- reactive({
      input$assay_name
    })

    observeEvent(input$instructions, {
      showModal(
        modalDialog(
          title = "Instructions",
          # nolint start
          instructions(
            annots_link = config::get("annotations_link"),
            templates_link = config::get("templates_link")
          ),
          # nolint end
          easyClose = TRUE
        )
      )
    })

    ##############################
    ####  Validation Results  ####
    ##############################

    ## Perform checks

    # Missing columns ----------------------------------------------------------
    missing_cols_indiv <- reactive({
      check_cols_individual(indiv(), indiv_template())
    })
    missing_cols_biosp <- reactive({
      check_cols_biospecimen(biosp(), biosp_template())
    })
    missing_cols_assay <- reactive({
      check_cols_assay(assay(), assay_template())
    })
    missing_cols_manifest <- reactive({
      check_cols_manifest(
        manifest(),
        config::get("templates")$manifest_template
      )
    })

    # Individual and specimen IDs match ----------------------------------------
    individual_ids_indiv_biosp <- reactive({
      check_indiv_ids_match(indiv(), biosp(), "individual", "biospecimen")
    })
    individual_ids_indiv_manifest <- reactive({
      check_indiv_ids_match(
        indiv(),
        manifest(),
        "individual",
        "manifest",
        bidirectional = FALSE
      )
    })
    specimen_ids_biosp_assay <- reactive({
      check_specimen_ids_match(biosp(), assay(), "biospecimen", "assay")
    })
    specimen_ids_biosp_manifest <- reactive({
      check_specimen_ids_match(
        biosp(),
        manifest(),
        "biospecimen",
        "manifest",
        bidirectional = FALSE
      )
    })

    # Annotation keys in manifest are valid ------------------------------------
    annotation_keys_manifest <- reactive({
      check_annotation_keys(
        manifest(),
        annots,
        whitelist_keys = c("path", "parent")
      )
    })

    # Annotation values in manifest and metadata are valid ---------------------
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

    # Individual and specimen IDs are not duplicated ---------------------------
    duplicate_indiv_ids <- reactive({
      check_indiv_ids_dup(indiv())
    })
    duplicate_specimen_ids <- reactive({
      check_specimen_ids_dup(biosp())
    })

    # Empty columns produce warnings -------------------------------------------
    empty_cols_manifest <- reactive({
      check_cols_empty(
        manifest(),
        success_msg = "No columns are empty in the manifest",
        fail_msg = "Some columns are empty in the manifest"
      )
    })
    empty_cols_indiv <- reactive({
      check_cols_empty(
        indiv(),
        success_msg = "No columns are empty in the individual metadata",
        fail_msg = "Some columns are empty in the individual metadata"
      )
    })
    empty_cols_biosp <- reactive({
      check_cols_empty(
        biosp(),
        success_msg = "No columns are empty in the biospecimen metadata",
        fail_msg = "Some columns are empty in the biospecimen metadata"
      )
    })
    empty_cols_assay <- reactive({
      check_cols_empty(
        assay(),
        success_msg = "No columns are empty in the assay metadata",
        fail_msg = "Some columns are empty in the assay metadata"
      )
    })

    # Incomplete required columns produce failures -----------------------------
    complete_cols_manifest <- reactive({
      check_cols_complete(
        manifest(),
        required_cols = config::get("complete_columns")$manifest,
        success_msg = "All required columns present are complete in the manifest", # nolint
        fail_msg = "Some required columns are incomplete in the manifest"
      )
    })
    complete_cols_indiv <- reactive({
      check_cols_complete(
        indiv(),
        required_cols = config::get("complete_columns")$individual,
        success_msg = "All required columns present are complete in the individual metadata", # nolint
        fail_msg = "Some required columns are incomplete in the individual metadata" # nolint
      )
    })
    complete_cols_biosp <- reactive({
      check_cols_complete(
        biosp(),
        required_cols = config::get("complete_columns")$biospecimen,
        success_msg = "All required columns present are complete in the biospecimen metadata", # nolint
        fail_msg = "Some required columns are incomplete in the biospecimen metadata" # nolint
      )
    })
    complete_cols_assay <- reactive({
      check_cols_complete(
        assay(),
        required_cols = config::get("complete_columns")$assay,
        success_msg = "All required columns present are complete in the assay metadata", # nolint
        fail_msg = "Some required columns are incomplete in the assay metadata" # nolint
      )
    })

    ## Show validation results on clicking "validate"
    ## Require that the study name is given; give error if not
    observeEvent(input$"validate_btn", {
      with_busy_indicator_server("validate_btn", {
        if (!is_study_name_valid(study_name())) {
          stop("Please check that study name is entered and only contains: letters, numbers, spaces, underscores, hyphens, periods, plus signs, and parentheses.") # nolint
        }
        ## Require at least one file input
        validate(
          need(
            any(
              !is.null(indiv()),
              !is.null(biosp()),
              !is.null(assay()),
              !is.null(manifest())
            ),
            message = "Please upload some data to validate"
          )
        )

        ## Upload only the files that have been given
        if (!is.null(indiv())) {
          save_to_synapse(
            files$indiv,
            parent = created_folder,
            name = files$indiv$name,
            annotations = list(
              study = study_name(),
              metadataType = "individual",
              species = species_name()
            )
          )
        }
        if (!is.null(biosp())) {
          save_to_synapse(
            files$biosp,
            parent = created_folder,
            name = files$biosp$name,
            annotations = list(
              study = study_name(),
              metadataType = "biospecimen",
              species = species_name()
            )
          )
        }
        if (!is.null(assay())) {
          save_to_synapse(
            files$assay,
            parent = created_folder,
            name = files$assay$name,
            annotations = list(
              study = study_name(),
              metadataType = "assay",
              assay = assay_name(),
              species = species_name()
            )
          )
        }
        if (!is.null(manifest())) {
          save_to_synapse(
            files$manifest,
            parent = created_folder,
            name = files$manifest$name,
            annotations = list(study = study_name())
          )
        }

        ## List results
        res <- list(
          missing_cols_indiv(),
          missing_cols_biosp(),
          missing_cols_assay(),
          missing_cols_manifest(),
          individual_ids_indiv_biosp(),
          individual_ids_indiv_manifest(),
          specimen_ids_biosp_assay(),
          specimen_ids_biosp_manifest(),
          annotation_keys_manifest(),
          annotation_values_manifest(),
          annotation_values_indiv(),
          annotation_values_biosp(),
          annotation_values_assay(),
          duplicate_indiv_ids(),
          duplicate_specimen_ids(),
          empty_cols_manifest(),
          empty_cols_indiv(),
          empty_cols_biosp(),
          empty_cols_assay(),
          complete_cols_manifest(),
          complete_cols_indiv(),
          complete_cols_biosp(),
          complete_cols_assay()
        )


        ## Populate validation report
        ## Successes box
        successes <- purrr::map_lgl(res, function(x) {
          inherits(x, "check_pass")
        })
        output$successes <- renderUI({
          report_results(res[successes], emoji_prefix = "check")
        })
        reporting_titles$success <- glue::glue("Successes ({sum(successes)})")

        ## Warnings box
        warnings <- purrr::map_lgl(res, function(x) {
          inherits(x, "check_warn")
        })
        output$warnings <- renderUI({
          report_results(
            res[warnings],
            emoji_prefix = "warning",
            verbose = TRUE
          )
        })
        reporting_titles$warn <- glue::glue("Warnings ({sum(warnings)})")

        ## Failures box
        failures <- purrr::map_lgl(res, function(x) {
          inherits(x, "check_fail")
        })
        output$failures <- renderUI({
          report_results(
            res[failures],
            emoji_prefix = "x",
            verbose = TRUE
          )
        })
        reporting_titles$fail <- glue::glue("Failures ({sum(failures)})")
      })
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
      skimr::skim_with(
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
          !is.null(vals()[[input$file_to_summarize]]),
          message = FALSE
        )
      )
      skimr::skim(vals()[[input$file_to_summarize]])
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
      visdat::vis_dat(vals()[[input$file_to_summarize]]) +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
    })
  })
}
