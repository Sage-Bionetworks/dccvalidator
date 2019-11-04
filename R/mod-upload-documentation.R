#' Upload documentation to Synapse
#'
#' This module creates a page that explains what documentation is needed to
#' describe the study and assay(s) that make up the study. It allows the user to
#' indicate the name of their study and upload documentation files.
#'
#' @keywords internal
#' @param id the module id
#' @param study_link_human html link to example of a study using human data
#' @param study_link_animal html link to example of a study using animal models
#' @return html ui for the module
upload_documents_ui <- function(id, study_link_human, study_link_animal) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    # Use shinyjs
    shinyjs::useShinyjs(),
    sidebarLayout(
      sidebarPanel(
        # UI for getting the study name
        get_study_ui(ns("doc_study")),

        # File import
        shinyjs::disabled(
          fileInput(
            ns("study_doc"),
            "Upload the study description file"
          )
        ),
        shinyjs::disabled(
          fileInput(
            ns("assay_doc"),
            "Upload the assay description files",
            multiple = TRUE
          )
        ),

        # Add an indicator feature to submit button
        with_busy_indicator_ui(
          shinyjs::disabled(
            actionButton(
              ns("upload_docs"),
              "Submit"
            )
          )
        )
      ),

      mainPanel(
        # Instructions/Description
        h3("Upload Study Documentation"),
        # nolint start
        p(
          "Study documentation gives data users an understanding of the cohort, model system, or other unit based on experimental design through which the data has been generated, and the methods used for the assays and/or assessments. This should be similar to a materials and methods section in a paper. An example of what this information should include can be found ",
          HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_animal}\">here</a> for an animal model study and ")),

          HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_human}\">here</a> for a human study."))
        ),
        h4("Study Description"),

        p("The study description is an overview of the study and should include:"),

        tags$ul(
          tags$li(
            "human studies",
            tags$ul(
              tags$li(
                "study type (randomized controlled study, prospective observational study, case-control study, or post-mortem study), disease focus, diagnostic criteria and inclusion/exclusion criteria of study participants. For post mortem studies, include the brain bank name(s) and links to website(s)."
              )
            )
          ),
          tags$li(
            "animal model studies",
            tags$ul(
              tags$li(
                "species, treatments, and if genetically modified-genotype and genetic background. Provide a link to the strain datasheet(s) if a commercial model, or a description of how it was created if not."
              )
            )
          ),
          tags$li(
            "in-vitro cell culture studies",
            tags$ul(
              tags$li(
                "species, cell type, and cell culture information such as primary or immortalized cell line, passage, treatments and other relevant information. If a commercial cell line provide a link."
              )
            )
          )
        ),

        p("Include references for more study information of available. Each study should be given both a descriptive and an abbreviated name. The abbreviation will be used to annotate all content associated with the study."),

        h4("Assay Description"),

        p(
          "For each assay, provide a summary of ",
          tags$b("sample processing, data generation,"),
          " and ",
          tags$b("data processing,"),
          " including which organs and tissues the samples came from. For other tests (such as cognitive assessments), include a description of how the test was done."
        )
        # nolint end
      )
    )
  )
}

#' Server function for upload_documentation module
#'
#' @keywords internal
#' @rdname upload_documents_ui
#' @param input the input from [shiny::callModule()]
#' @param output the output from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param parent_folder the Synapse folder to put a Documentation folder in
#' @param study_table_id synapse Id for the consortium study table
upload_documents_server <- function(input, output, session,
                                    parent_folder, study_table_id) {
  inputs_to_enable <- c(
    "doc_study",
    "study_doc",
    "assay_doc",
    "upload_docs"
  )
  purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

  # Create folder for upload
  docs_folder <- synapser::Folder(
    name = "Documentation",
    parent = parent_folder()
  )
  created_docs_folder <- synapser::synStore(docs_folder)

  # Get the study name
  study_name <- callModule(
    get_study_server,
    "doc_study",
    study_table_id = study_table_id
  )
  doc_annots <- reactive({
    list(study = study_name())
  })

  # Upload files to Synapse (after renaming them so they keep their original
  # names)
  observeEvent(input$upload_docs, {
    if (!is.null(input$study_doc) || !is.null(input$assay_doc)) {
      # When the button is clicked, wrap the code in the call to the
      # indicator server function
      with_busy_indicator_server("upload_docs", {
        if (!is_study_name_valid(study_name())) {
          stop("Please check that study name is entered and only contains: letters, numbers, spaces, underscores, hyphens, periods, plus signs, and parentheses.") # nolint
        }
        all_docs <- rbind(input$study_doc, input$assay_doc)
        all_datapaths <- all_docs$datapath
        all_names <- paste0(study_name(), "_", all_docs$name)
        docs <- purrr::map2(all_datapaths, all_names, function(x, y) {
          save_to_synapse(
            list(datapath = x, name = y),
            parent = created_docs_folder,
            name = y,
            annotations = doc_annots()
          )
        })
        shinyjs::reset("study_doc")
        shinyjs::reset("assay_doc")
      })
    }
  })
}
