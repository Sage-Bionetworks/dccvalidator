#' Validate metadata
#'
#' This module creates a page where users can upload their metadata files and
#' manifest, and see a report of validation results.
#'
#' @noRd
#' @import shiny
#' @param id the module id
#' @param species_list Vector of species user can choose from for their study
#' @param assay_templates Vector of assay template names
#' @param include_biospecimen_type TRUE to include radiobutton options for
#' specimen type of "in vitro" or "other (in vivo, postmortem)"; else FALSE
#' (default) to leave out of application.
#' @return html ui for the module
validator_ui <- function(id, species_list, assay_templates,
                         include_biospecimen_type = FALSE) {
  ns <- NS(id)

  # Validator tab UI
  tabItem(
    tabName = id,

    # Sidebar
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("instructions"), "Show instructions"),
        br(),
        br(),

        # UI for getting the study name
        get_study_ui(ns("study")),

        # Species
        div(
          class = "result",
          div(
            class = "wide",
            shinyjs::disabled(
              radioButtons(
                ns("species"),
                "Species",
                species_list
              )
            )
          ),
          popify(
            tags$a(icon(name = "question-circle"), href = "#"),
            "Information",
            "Select the species used in the study.",
            placement = "left",
            trigger = "hover"
          )
        ),

        # Biospecimen type
        if (include_biospecimen_type) {
          conditionalPanel(
            condition = "input.species != 'drosophila'",
            div(
              class = "result",
              div(
                class = "wide",
                shinyjs::hidden(
                  shinyjs::disabled(
                    radioButtons(
                      ns("biospecimen_type"),
                      "Biospecimen Type",
                      choices = NA
                    )
                  )
                )
              ),
              popify(
                tags$a(icon(name = "question-circle"), href = "#"),
                "Information",
                "Select the specimen type: in vitro, in vivo or postmortem.",
                placement = "left",
                trigger = "hover"
              )
            ),
            ns = ns
          )
        },

        # Assay name
        div(
          class = "result",
          div(
            class = "wide",
            shinyjs::disabled(
              selectInput(
                ns("assay_name"),
                "Assay type",
                names(assay_templates)
              )
            )
          ),
          popify(
            tags$a(icon(name = "question-circle"), href = "#"),
            "Information",
            "Select the type of assay that matches your assay metadata.", # nolint
            placement = "left",
            trigger = "hover"
          )
        ),

        # Files to be validated
        conditionalPanel(
          condition = "input.species != 'drosophila'",
          div(
            class = "result",
            div(
              class = "wide",
              shinyjs::disabled(
                fileInput(
                  ns("indiv_meta"),
                  "1kD_standardized_demographic_data file (.csv)",
                  width = NULL,
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  )
                )
              )
            ),
            popify(
              tags$a(icon(name = "question-circle"), href = "#"),
              "Information",
              "Select the individual metadata file. This file should have one row per individual, with data about each individual in the experiment. If adding a new dataset to an existing dataset, please include all previous individuals.", # nolint
              placement = "left",
              trigger = "hover"
            )
          ),
          ns = ns
        ),
        div(
          class = "result",
          div(
            class = "wide",
            shinyjs::disabled(
              fileInput(
                ns("biosp_meta"),
                "Biospecimen metadata file (.csv)",
                width = NULL,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              )
            )
          ),
          popify(
            tags$a(icon(name = "question-circle"), href = "#"),
            "Information",
            "Select the biospecimen metadata file. This file should have one row per specimen, with data about each specimen in the experiment. If adding a new dataset to an existing dataset, please include all previous specimens.", # nolint
            placement = "left",
            trigger = "hover"
          )
        ),
        div(
          class = "result",
          div(
            class = "wide",
            shinyjs::disabled(
              fileInput(
                ns("assay_meta"),
                "Assay metadata file (.csv)",
                width = NULL,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv"
                )
              )
            )
          ),
          popify(
            tags$a(icon(name = "question-circle"), href = "#"),
            "Information",
            "Select the assay metadata file. Depending on the assay, this file should have one row per specimen or one row per individual (indicated in the template), with data about the assay performed on each specimen or individual in the experiment. If adding a new dataset to an existing dataset, please include all previous assay specimens or individuals. Please be sure to choose the correct assay type from the drop-down above, as well.", # nolint
            placement = "left",
            trigger = "hover"
          )
        ),
        div(
          class = "result",
          div(
            class = "wide",
            shinyjs::disabled(
              fileInput(
                ns("manifest"),
                "Upload Manifest File (.tsv or .txt)",
                multiple = FALSE,
                accept = c(
                  "text/tsv",
                  "text/tab-separated-values,text/plain",
                  ".tsv"
                )
              )
            )
          ),
          popify(
            tags$a(icon(name = "question-circle"), href = "#"),
            "Information",
            "Select the manifest file. This file should have one row per file to be uploaded to Synapse, including the metadata files, with data about the contents of each file, as well as the study itself. The manifest will be used to upload the data.", # nolint
            placement = "left",
            trigger = "hover"
          )
        ),

        # Add an indicator feature to validate button
        with_busy_indicator_ui(
          shinyjs::disabled(
            actionButton(
              ns("validate_btn"),
              "Validate"
            )
          )
        ),
        hr(),
        # Add button to reset the form
        shinyjs::disabled(
          actionButton(
            ns("reset_btn_validate"),
            "Reset"
          )
        )
      ),

      # Main panel
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Validation Results",
            br(),
            results_boxes_ui(ns("validation_results"))
          ),
          tabPanel(
            "Data Summary",
            fluidRow(
              shinydashboard::box(
                title = "Dataset summary",
                valueBoxOutput(ns("nindividuals")),
                valueBoxOutput(ns("nspecimens")),
                valueBoxOutput(ns("ndatafiles")),
                hr(),
                file_summary_ui(ns("file_summary")),
                width = 12
              )
            )
          )
        )
      )
    )
  )
}

#' Server function for validator module
#'
#' @noRd
#' @inheritParams get_synapse_table
#' @inheritParams create_folder
#' @inheritParams validator_ui
#' @param input the input from [shiny::callModule()]
#' @param output the output from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param annotations_table Synapse ID of table containing annotation
#' definitions
#' @param annots_link Link to learn more about annotations
#' @param templates_link Link to location of metadata templates
#' @param contact_email Email address to contact with questions
validator_server <- function(input, output, session, study_names, species_list,
                             assay_templates, annotations_table, annots_link,
                             templates_link, contact_email, parent,
                             synapseclient, syn,
                             include_biospecimen_type = FALSE) {

  ## Initial titles for report boxes
  callModule(results_boxes_server, "validation_results", results = NULL)

  study_name <- callModule(
    get_study_server,
    "study",
    study_names = study_names
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
  if (include_biospecimen_type) {
    inputs_to_enable <- c(inputs_to_enable, "biospecimen_type")
  }
  purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

  ## Reset fileInputs, study name, and other inputs
  observeEvent(input$reset_btn_validate, {
    reset_inputs("indiv_meta", "biosp_meta", "assay_meta", "manifest")
    files$indiv <- NULL
    files$biosp <- NULL
    files$assay <- NULL
    files$manifest <- NULL
    callModule(results_boxes_server, "validation_results", NULL)
    study_name <- callModule(
      get_study_server,
      "study",
      study_names = study_names,
      reset = TRUE
    )
    updateRadioButtons(
      session,
      "species",
      "Species",
      species_list
    )
    updateSelectInput(
      session,
      "assay_name",
      "Assay type",
      names(assay_templates)
    )
    specimen_types <- unique(
      names(
        get_golem_config("templates")$biospecimen[[input$species]]
      )
    )
    if (!is.null(specimen_types)) {
      # Grab specimen types from config and default choose first in list
      updateRadioButtons(
        session,
        "biospecimen_type",
        "Biospecimen Type",
        choices = specimen_types,
        selected = specimen_types[1]
      )
      shinyjs::show("biospecimen_type")
    } else {
      shinyjs::hide("biospecimen_type")
      updateRadioButtons(
        session,
        "biospecimen_type",
        "Specimen Type",
        choices = "",
        selected = ""
      )
    }
  })

  ## If drosophila species checked, reset fileInput
  ## Change Specimen Type radioButtons depending on species
  observeEvent(input$species, {
    if (input$species == "drosophila") {
      reset_inputs("indiv_meta")
      files$indiv <- NULL
      # biospecimen type will hide automatically, but need to update the values
      updateRadioButtons(
        session,
        "biospecimen_type",
        "Specimen Type",
        choices = "",
        selected = ""
      )
    } else {
      specimen_types <- unique(
        names(
          get_golem_config("templates")$biospecimen[[input$species]]
        )
      )
      if (!is.null(specimen_types)) {
        # Grab specimen types from config and default choose first in list
        updateRadioButtons(
          session,
          "biospecimen_type",
          "Biospecimen Type",
          choices = specimen_types,
          selected = specimen_types[1]
        )
        shinyjs::show("biospecimen_type")
      } else {
        shinyjs::hide("biospecimen_type")
        updateRadioButtons(
          session,
          "biospecimen_type",
          "Specimen Type",
          choices = "",
          selected = ""
        )
      }
    }
  })

  ## Download annotation definitions
  annots <- purrr::map_dfr(
    annotations_table,
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
  biospecimen_type <- reactive({
    NA
  })
  if (get_golem_config("include_biospecimen_type")) {
    biospecimen_type <- reactive({
      input$biospecimen_type
    })
  }
  assay_name <- reactive({
    input$assay_name
  })

  observeEvent(input$instructions, {
    showModal(
      modalDialog(
        title = "Instructions",
        # nolint start
        instructions(
          annots_link = annots_link,
          templates_link = templates_link
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
      ## Check for any invalid characters in files before continuing
      invalid_checks <- check_all_invalid_char(
        manifest = manifest(),
        indiv = indiv(),
        biosp = biosp(),
        assay = assay()
      )
      has_invalid <- purrr::map_lgl(
        invalid_checks,
        ~ inherits(., "check_fail")
      )
      validate(
        need(
          !any(has_invalid),
          message = summarize_invalid_char_check(invalid_checks)
        )
      )

      ## Upload only the files that have been given
      if (!is.null(indiv())) {
        save_to_synapse(
          files$indiv,
          parent = parent,
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
          parent = parent,
          annotations = list(
            study = study_name(),
            metadataType = "biospecimen",
            species = species_name(),
            biospecimenType = biospecimen_type(),
            isDocumentation = FALSE
          ),
          synapseclient = synapse,
          syn = syn
        )
      }
      if (!is.null(assay())) {
        save_to_synapse(
          files$assay,
          parent = parent,
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
          parent = parent,
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
        ),
        "template" = c(
          gather_template_ids(type = "manifest"),
          gather_template_ids(type = "individual", species = species_name()),
          gather_template_ids(
            type = "biospecimen",
            species = species_name(),
            biospecimen_type = biospecimen_type()
          ),
          gather_template_ids(type = "assay", assay = assay_name())
        )
      )
      res <- check_all(
        data = all_data,
        annotations = annots,
        study = study_name(),
        syn = syn,
        samples_table = get_golem_config("samples_table")
      )

      callModule(results_boxes_server, "validation_results", res)

      # Give next step if no failures
      next_step_modal(res, contact_email)
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
}
