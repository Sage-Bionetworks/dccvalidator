#' Upload documentation to Synapse
#'
#' This module creates a page that explains what documentation is needed to
#' describe the study and assay(s) that make up the study. It allows the user to
#' indicate the name of their study and upload documentation files.
#'
#' @noRd
#' @param id the module id
#' @param study_link_human html link to example of a study using human data
#' @param study_link_animal html link to example of a study using animal models
#' @param study_link_ref html link to example of study acknowledgment
#'   preference
#' @return html ui for the module
upload_documents_ui <- function(id, study_link_human,
                                study_link_animal, study_link_ref) {
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
        upload_docs_instruct_text(
          study_link_human,
          study_link_animal,
          study_link_ref
        )
      )
    )
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
    list(study = study_name())
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

#' Create instructions for uploading documentation
#'
#' This function creates the instruction text for the Documentation
#' portion of dccvalidator. The study_link_ref argument dictates
#' whether instructions for uploading an acknowledgment is added.
#' If study_link_ref is an empty string, then the acknowledgement
#' instructions are not added.
#'
#' @noRd
#' @inheritParams upload_documents_ui
#' @return html taglist with instructions
upload_docs_instruct_text <- function(study_link_human, study_link_animal,
                                      study_link_ref) {
  if (study_link_ref != "") {
    overview_text <- p(
      # nolint start
      "This information should be similar to a materials and methods section in a paper. An example of what a study should include can be found ",
      HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_animal}\">here</a> for an animal model study and ")),
      HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_human}\">here</a> for a human study.")),
      "If you wish, also provide an acknowledgement statment and/or reference that should be included in publications resulting from secondary data use; examples can be found ",
      HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_ref}\">here</a>.")),
      "This can be provided as part of the study documentation text."
      # nolint end
    )
  } else {
    overview_text <- p(
      # nolint start
      "This information should be similar to a materials and methods section in a paper. An example of what a study should include can be found ",
      HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_animal}\">here</a> for an animal model study and ")),
      HTML(glue::glue("<a target =\"_blank\" href=\"{study_link_human}\">here</a> for a human study."))
      # nolint end
    )
  }

  tagList(
    # Instructions/Description
    h3("Upload Study Documentation"),
    # nolint start
    overview_text,
    h4("Study Description"),

    p("Each study should be given both a descriptive and an abbreviated name. The abbreviation will be used to annotate all content associated with the study. For a study with a human cohort, the study description should include:"),
    tags$ul(
      tags$li(
        "study type (randomized controlled study, prospective observational study, case-control study, or post-mortem study)"
      ),
      tags$li(
        "disease focus"
      ),
      tags$li(
        "diagnostic criteria and inclusion/exclusion criteria of study participants"
      ),
      tags$li(
        "(for post mortem studies) the brain bank name(s) and links to website(s)"
      )
    ),

    p("For a study with an animal model cohort, the study description should include:"),
    tags$ul(
      tags$li(
        "species"
      ),
      tags$li(
        "treatments"
      ),
      tags$li(
        "(if genetically modified) genotype and genetic background. Provide a link to the strain datasheet(s) if a commercial model, or a description of how it was created if not."
      )
    ),

    p("For studies using in-vitro cell culture, the study description should include:"),
    tags$ul(
      tags$li(
        "species"
      ),
      tags$li(
        "cell type"
      ),
      tags$li(
        "cell culture information (such as primary or immortalized cell line, passage, treatments, differentiation). If a commercial cell line, provide a link."
      )
    ),

    p("Include citations for more study information if available."),

    h4("Assay Description"),

    p(
      "For each assay, provide a summary of ",
      tags$b("sample processing, data generation,"),
      " and ",
      tags$b("data processing,"),
      " including which organs and tissues the samples came from. For other tests (such as cognitive assessments or imaging), include a description of how the test was done. Include links for any commercial equipment or tools, code repositories, and citations for more information, if available."
    ),
    p(
      "Detailed protocols are highly recommended. These can be uploaded as pdf together with the data-files, or as links to protocol repositories such as ",
      tags$a(href = "https://www.protocols.io", "protocols.io", target = "_blank"),
      (" or "),
      tags$a(href = "https://theolb.readthedocs.io/en/latest/index.html#", "Open Lab Book.", target = "_blank")
    )
    # nolint end
  )
}
