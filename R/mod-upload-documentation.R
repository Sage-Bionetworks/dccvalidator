#' Upload documentation to Synapse
#'
#' This module creates a page that explains what documentation is needed to
#' describe the study and assay(s) that make up the study. This module is
#' customizable, both for the displayed documentation (via `markdown_path`) and
#' for including a sidebar-style widget that allows for documentation upload
#' (via `include_widget`). The upload widget allows the user to
#' indicate the name of their study and upload documentation files.
#'
#' @noRd
#' @param id the module id
#' @param markdown_path path to the markdown file to be displayed
#' @param include_widget TRUE if the upload widget should be included,
#'   else FALSE
#' @return html ui for the module
upload_documents_ui <- function(id, markdown_path, include_widget) {
  ns <- NS(id)

  tabItem(
    tabName = id,
    # Use shinyjs
    shinyjs::useShinyjs(),
    if (include_widget) {
      sidebarLayout(
        sidebarPanel(
          # UI for getting the study name
          get_study_ui(ns("doc_study")),

          # File import
          div(
            class = "result",
            div(
              class = "wide",
              shinyjs::disabled(
                fileInput(
                  ns("study_doc"),
                  "Upload study description file (.txt, .docx, .md, .pdf, .tex)",
                  accept = c(
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document", # nolint
                    "application/msword",
                    "application/pdf",
                    "text/plain",
                    "application/x-tex",
                    "text/markdown"
                  )
                )
              )
            ),
            popify(
              tags$a(icon(name = "question-circle"), href = "#"),
              "Information",
              "Select the study description file. Please refer to the information on this page to learn what should be in the study description.", # nolint
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
                  ns("assay_doc"),
                  "Upload assay description files (.txt, .docx, .md, .pdf, .tex)",
                  multiple = TRUE,
                  accept = c(
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document", # nolint
                    "application/msword",
                    "application/pdf",
                    "text/plain",
                    "application/x-tex",
                    "text/markdown"
                  )
                )
              )
            ),
            popify(
              tags$a(icon(name = "question-circle"), href = "#"),
              "Information",
              "Select the assay description file(s). Please refer to the information on this page to learn what should be in the assay description.", # nolint
              placement = "left",
              trigger = "hover"
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
          ),
          hr(),
          shinyjs::disabled(
            actionButton(
              ns("reset_btn_doc"),
              "Reset"
            )
          )
        ),
        mainPanel(
          div(
            get_markdown(markdown_path = markdown_path)
          ) 
        )
      )
    } else {
      div(
        get_markdown(markdown_path = markdown_path)
      )
    }
  )
}

#' Server function for upload_documentation module
#'
#' @noRd
#' @rdname upload_documents_ui
#' @inheritParams get_synapse_table
#' @inheritParams create_folder
#' @param input the input from [shiny::callModule()]
#' @param output the output from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param parent_folder the Synapse folder to put a Documentation folder in
#' @param study_names vector of study names
upload_documents_server <- function(input, output, session,
                                    parent_folder, study_names,
                                    synapseclient, syn) {
  inputs_to_enable <- c(
    "doc_study",
    "study_doc",
    "assay_doc",
    "upload_docs",
    "reset_btn_doc"
  )
  purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

  # Create folder for upload
  docs_folder <- synapseclient$Folder(
    name = "Documentation",
    parent = parent_folder()
  )
  created_docs_folder <- syn$store(docs_folder)

  # Get the study name
  study_name <- callModule(
    get_study_server,
    "doc_study",
    study_names = study_names
  )
  doc_annots <- reactive({
    list(
      study = study_name(),
      isDocumentation = TRUE
    )
  })

  # Control inputs by storing in reactiveValues
  docs <- reactiveValues(
    study = NULL,
    assay = NULL
  )
  observeEvent(input$study_doc, {
    docs$study <- input$study_doc
  })
  observeEvent(input$assay_doc, {
    docs$assay <- input$assay_doc
  })

  # Reset tab
  observeEvent(input$reset_btn_doc, {
    docs$study <- NULL
    docs$assay <- NULL
    reset_inputs("study_doc", "assay_doc")
    study_name <- callModule(
      get_study_server,
      "doc_study",
      study_names = study_names,
      reset = TRUE
    )
  })

  # Upload files to Synapse (after renaming them so they keep their original
  # names)
  observeEvent(input$upload_docs, {
    # When the button is clicked, wrap the code in the call to the
    # indicator server function
    with_busy_indicator_server("upload_docs", {
      if (is.null(docs$study) && is.null(docs$assay)) {
        stop("Please provide files to upload.")
      }
      if (!is_name_valid(study_name())) {
        stop("Please check that study name is entered and only contains: letters, numbers, spaces, underscores, hyphens, periods, plus signs, and parentheses.") # nolint
      }
      all_docs <- rbind(docs$study, docs$assay)
      all_datapaths <- all_docs$datapath
      all_names <- paste0(study_name(), "_", all_docs$name)
      docs <- purrr::map2(all_datapaths, all_names, function(x, y) {
        save_to_synapse(
          list(datapath = x, name = y),
          parent = created_docs_folder,
          annotations = doc_annots(),
          synapseclient = synapseclient,
          syn = syn
        )
      })
      docs$study <- NULL
      docs$assay <- NULL
      reset_inputs("study_doc", "assay_doc")
    })
  })
}
