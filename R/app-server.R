#' App server
#'
#' Create the server-side component of the dccvalidator Shiny app.
#'
#' @import shiny
#' @import shinydashboard
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @return none
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_server <- function(input, output, session) {
  syn <- synapse$Synapse()

  ## Initial titles for report boxes
  callModule(results_boxes_server, "Validation Results", results = NULL)

  session$sendCustomMessage(type = "readCookie", message = list())

  observeEvent(input$cookie, {
    is_logged_in <- FALSE
    tryCatch({
      syn$login(sessionToken = input$cookie)
      is_logged_in <- TRUE

      ### update waiter loading screen once login successful
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
        )
      )
      Sys.sleep(2)
      waiter::waiter_hide()
    }, error = function(err) {
      Sys.sleep(2)
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("log in", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    })
    req(is_logged_in)

    ## Check if user is in AMP-AD Consortium team (needed in order to create
    ## folder at the next step), and if they are a certified user.
    user <- syn$getUserProfile()
    membership <- check_team_membership(
      teams = config::get("teams"),
      user = user,
      syn = syn
    )
    certified <- check_certified_user(user$ownerId, syn = syn)
    report_unsatisfied_requirements(membership, certified, syn = syn)

    ## If user is a member of the team(s), create folder to save files and
    ## enable inputs
    if (inherits(membership, "check_pass") &
      inherits(certified, "check_pass")) {
      created_folder <- try(
        create_folder(
          parent = config::get("parent"),
          name = user$userName,
          synapseclient = synapse,
          syn = syn
        )
      )

      all_studies <- get_study_names(reactive(config::get("study_table")), syn)
      study_name <- callModule(
        get_study_server,
        "study",
        study_names = all_studies
      )

      inputs_to_enable <- c(
        "indiv_meta",
        "biosp_meta",
        "assay_meta",
        "manifest",
        "species",
        "assay_name",
        "validate_btn",
        "reset_btn_validate"
      )
      purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

      # Documentation server needs created_folder to run correctly
      callModule(
        upload_documents_server,
        "documentation",
        parent_folder = reactive(created_folder),
        study_names = all_studies,
        synapseclient = synapse,
        syn = syn
      )
    }

    ## Reset fileInputs, study name, and other inputs
    observeEvent(input$reset_btn_validate, {
      reset_inputs("indiv_meta", "biosp_meta", "assay_meta", "manifest")
      files$indiv <- NULL
      files$biosp <- NULL
      files$assay <- NULL
      files$manifest <- NULL
      callModule(results_boxes_server, "Validation Results", list(NULL))
      study_name <- callModule(
        get_study_server,
        "study",
        study_names = all_studies,
        reset = TRUE
      )
      updateRadioButtons(
        session,
        "species",
        "Species",
        config::get("species_list")
      )
      updateSelectInput(
        session,
        "assay_name",
        "Assay type",
        names(config::get("templates")$assay_templates)
      )
    })

    ## If drosophila species checked, reset fileInput
    observeEvent(input$species, {
      if (input$species == "drosophila") {
        reset_inputs("indiv_meta")
        files$indiv <- NULL
      }
    })

    ## Download annotation definitions
    annots <- purrr::map_dfr(
      config::get("annotations_table"),
      get_synapse_annotations,
      syn = syn
    )

    ## Store files in separate variable to be able to reset inputs to NULL
    files <- reactiveValues(
      indiv = NULL,
      manifest = NULL,
      biosp = NULL,
      assay = NULL
    )
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
      readr::read_tsv(files$manifest$datapath)
    })
    indiv <- reactive({
      if (is.null(files$indiv)) {
        return(NULL)
      }
      readr::read_csv(files$indiv$datapath)
    })
    biosp <- reactive({
      if (is.null(files$biosp)) {
        return(NULL)
      }
      readr::read_csv(files$biosp$datapath)
    })
    assay <- reactive({
      if (is.null(files$assay)) {
        return(NULL)
      }
      readr::read_csv(files$assay$datapath)
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

    ## Show validation results on clicking "validate"
    ## Require that the study name is given; give error if not
    observeEvent(input$"validate_btn", {
      with_busy_indicator_server("validate_btn", {
        if (!is_name_valid(study_name())) {
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
          ),
          need(
            is.null(manifest()) ||
              tolower(tools::file_ext(input$manifest$name)) != "csv",
            "Manifest file must be .tsv or .txt, not .csv"
          )
        )

        ## Upload only the files that have been given
        if (!is.null(indiv())) {
          save_to_synapse(
            files$indiv,
            parent = created_folder,
            annotations = list(
              study = study_name(),
              metadataType = "individual",
              species = species_name(),
              isDocumentation = FALSE
            ),
            synapseclient = synapse,
            syn = syn
          )
        }
        if (!is.null(biosp())) {
          save_to_synapse(
            files$biosp,
            parent = created_folder,
            annotations = list(
              study = study_name(),
              metadataType = "biospecimen",
              species = species_name(),
              isDocumentation = FALSE
            ),
            synapseclient = synapse,
            syn = syn
          )
        }
        if (!is.null(assay())) {
          save_to_synapse(
            files$assay,
            parent = created_folder,
            annotations = list(
              study = study_name(),
              metadataType = "assay",
              assay = assay_name(),
              species = species_name(),
              isDocumentation = FALSE
            ),
            synapseclient = synapse,
            syn = syn
          )
        }
        if (!is.null(manifest())) {
          save_to_synapse(
            files$manifest,
            parent = created_folder,
            annotations = list(
              study = study_name(),
              metadataType = "manifest",
              species = species_name(),
              isDocumentation = FALSE
            ),
            synapseclient = synapse,
            syn = syn
          )
        }

        ## Load in data to table for validation checks
        ## If file name is NULL, pass in NA.
        all_data <- tibble::tibble(
          "metadataType" = c(
            "manifest",
            "individual",
            "biospecimen",
            "assay"
          ),
          "name" = c(
            files$manifest$name %||% NA,
            files$indiv$name %||% NA,
            files$biosp$name %||% NA,
            files$assay$name %||% NA
          ),
          "species" = species_name(),
          "assay" = assay_name(),
          "file_data" = c(
            list(manifest()),
            list(indiv()),
            list(biosp()),
            list(assay())
          )
        )

        res <- check_all(
          data = all_data,
          annotations = annots,
          study = study_name(),
          syn
        )

        callModule(results_boxes_server, "Validation Results", res)

        # Give next step if no failures
        next_step_modal(res, config::get("contact_email"))
      })
    })

    ## Counts of individuals, specimens, and files
    output$nindividuals <- renderValueBox({
      valueBox(
        count_unique_values(
          indiv()$individualID,
          biosp()$individualID,
          manifest()$individualID
        ),
        "Individuals",
        icon = icon("users")
      )
    })

    output$nspecimens <- renderValueBox({
      valueBox(
        count_unique_values(
          biosp()$specimenID,
          assay()$specimenID,
          manifest()$specimenID
        ),
        "Specimens",
        icon = icon("vial")
      )
    })

    output$ndatafiles <- renderValueBox({
      valueBox(
        count_unique_values(manifest()$path),
        "Data files in manifest",
        icon = icon("file")
      )
    })

    observe({
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

      callModule(
        file_summary_server,
        "file_summary",
        vals
      )
    })
  })
}

#' Modal with next step
#'
#' If none of the checks inherit `check_fail`, then pop up a modal to tell
#' users what to do next.
#'
#' @param results List of conditions
#' @param email Contact email as a string
#' @noRd
next_step_modal <- function(results, email) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  if (!any(is_failure)) {
    showModal(
      modalDialog(
        title = "Great work!",
        HTML(
          glue::glue(
            "Your validated file(s) had no failures. Please contact <a target=\"_blank\" href=\"{email}\">{email}</a> to proceed with the next step if you have validated all finalized metadata and manifest files at once. For multiple assays, please validate each assay with your other metadata files (individual and/or biospecimen) and manifest." # nolint
          )
        ),
        easyClose = TRUE
      )
    )
  }
}
