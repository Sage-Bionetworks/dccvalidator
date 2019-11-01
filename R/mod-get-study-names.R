#' Gather studies within consortium
#'
#' @keywords internal
#' @rdname get_study_ui
#' @param study_table_id synapse Id for the consortium study table
#' @return list of studies
get_study_names <- function(study_table_id) {
  study_table <- syndccutils::get_table_df(study_table_id())
  return(c("", sort(study_table$StudyName)))
}

#' UI for the get_study module
#'
#' @keywords internal
#' @param id the id
#' @return html UI for get_study panel
get_study_ui <- function(id) {
  ns <- NS(id)
  # Use shinyjs
  shinyjs::useShinyjs()

  tagList(
    # Ability to choose to add to existing study
    shinyjs::disabled(
      radioButtons(
        ns("study_exists"),
        "Does the study currently exist?",
        choices = c("Yes", "No"),
        selected = "Yes"
      )
    ),
    conditionalPanel(
      condition = "input.study_exists == 'Yes'",
      ns = ns,
      shinyjs::disabled(
        selectInput(
          ns("study_choice"),
          "Choose the study",
          ""
        )
      )
    ),
    conditionalPanel(
      condition = "input.study_exists == 'No'",
      ns = ns,
      shinyjs::disabled(
        textInput(
          ns("study_text"),
          "Enter the study name"
        )
      )
    )
  )
}

#' Server function for the get_study module that gives the study
#' name given by the user
#'
#' Get study name based on either:
#' - existing study selection box
#' - new name for currently non-existing study
#'
#' @keywords internal
#' @rdname get_study_ui
#' @param input the input variables from [shiny::callModule()]
#' @param output the output variables from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param study_table_id synapse Id for the consortium study table
#' @return name of the study
get_study_server <- function(input, output, session, study_table_id) {
  all_studies <- get_study_names(study_table_id)
  # The session passed in from callModule() does not work for updating
  # the input selection choices; manually get session
  session <- getDefaultReactiveDomain()
  updateSelectInput(
    session,
    inputId = "study_choice",
    choices = all_studies
  )

  # Enable the inputs
  inputs_to_enable <- c(
    "study_exists",
    "study_choice",
    "study_text"
  )
  purrr::walk(inputs_to_enable, function(x) shinyjs::enable(x))

  study_name <- reactiveVal(NULL)
  # Even though study_name is a reactive value,
  # need observe to make sure it updates based on input
  observe({
    if (input$study_exists == "Yes") {
      study_name(input$study_choice)
    } else {
      study_name(input$study_text)
    }
  })
  observeEvent(input$study_exists, {
    if (input$study_exists == "No") {
      study_name("")
    }
  })
  return(study_name)
}
