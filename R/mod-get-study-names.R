#' Gather studies within consortium
#' @param study_table_id synapse Id for the consortium study table
#' @return list of studies
get_study_names <- function(study_table_id) {
  table_id <- study_table_id
  study_table <- syndccutils::get_table_df(table_id)
  return(sort(study_table$StudyName))
}

#' UI for the get_study module
#' @param id the id
#' @param study_table_id synapse Id for the consortium study table
#' @return html UI for get_study panel
get_study_ui <- function(id, study_table_id) {
  ns <- NS(id)
  tagList(
    # Ability to choose to add to existing study
    radioButtons(
      ns("study_exists"),
      "Does the study currently exist?",
      choices = c("Yes", "No"),
      selected = "Yes"
    ),
    conditionalPanel(
      condition = "input.study_exists == 'Yes'",
      ns = ns,
      selectInput(
        ns("study_choice"),
        "Choose the study",
        get_study_names(study_table_id)
      )
    ),
    conditionalPanel(
      condition = "input.study_exists == 'No'",
      ns = ns,
      textInput(
        ns("study_text"),
        "Enter the study name"
      )
    )
  )
}

#' Server function for the get_study module
#' Get study name based on either:
#' - existing study selection box
#' - new name for currently non-existing study
#' @param input the input variables from [shiny::callModule()]
#' @param output the output variables from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @return name of the study
get_study_server <- function(input, output, session) {
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
  return(study_name)
}
