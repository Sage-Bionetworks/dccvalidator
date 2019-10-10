#' UI function for the upload_documentation module
#' @param id the module id
#' @return html ui for the module
upload_documents_ui <- function(id) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    # Use shinyjs
    shinyjs::useShinyjs(),

    # Instructions/Description
    h3("Upload Unstructured Metadata"),
    # nolint start
    p("Unstructured metadata is similar to the materials and methods in a paper. These are used in the portal to give a summary of the project, assays, and other relevant information. An example of what this information should include and how it will appear in the portal can be found ", tags$a(href = "https://adknowledgeportal.synapse.org/#/Explore/Studies?Study=syn8391648", "here"), "."),

    h4("Study Description"),

    p("The study description is an overview of the study and should include:"),

    tags$ul(
      tags$li("human studies", tags$ul(tags$li("how the data was obtained, as well as a summary description of the data, including study type (prospective cohort, case-control, or post-mortem), disease focus, inclusion/exclusion criteria, and number of participants or donors. For post mortem studies, include the brain bank name(s) and tissue(s) that were sampled."))),
      tags$li("model studies", tags$ul(tags$li("where the models were generated, as well as a summary description of the model, including common name, genetic background, and a link to the strain datasheet, or datasheets if a cross between two strains.")))
    ),

    h4("Assay Description"),

    p("The assay description should include a summary of ", tags$b("sample processing, data generation,"), " and ", tags$b("data processing.")),
    # nolint end

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
  )
}

#' Server function for upload_documentation module
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
    if (study_name() != "") {
      if (!is.null(input$study_doc) || !is.null(input$assay_doc)) {
        # When the button is clicked, wrap the code in the call to the
        # indicator server function
        with_busy_indicator_server("upload_docs", {
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
        })
      }
    }
  })
}
