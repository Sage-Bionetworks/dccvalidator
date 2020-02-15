#' UI for the file summary module
#'
#' Creates the UI for the file summary module, complete with a drop-down
#' selection box, and two tabs, one for a file overview and one for file
#' details.
#'
#' @export
#' @rdname file_summary
#' @param id the id
#' @return html UI for file summary
#' @examples
#' library("shiny")
#' library("shinydashboard")
#'
#' server <- function(input, output) {
#'   # Create some simple file dataa
#'   data <- reactive({list(
#'     "individual" = data.frame(
#'       individualID = c("a", "b", "c"),
#'       age = c(23, 24, 24),
#'       stringsAsFactors = FALSE
#'     ),
#'     "biospecimen" = data.frame(
#'       individualID = c("a", "b", "c"),
#'       specimenID = c("a1", "b1", "c1"),
#'       isReal = c(FALSE, FALSE, FALSE),
#'       stringsAsFactors = FALSE
#'     )
#'   )})
#'   # Show file summary
#'   callModule(file_summary_server, "summary", file_data = data)
#' }
#'
#' ui <- function(request) {
#'   dashboardPage(
#'     header = dashboardHeader(),
#'     sidebar = dashboardSidebar(),
#'     body = dashboardBody(
#'       includeCSS(
#'         system.file("app/www/custom.css", package = "dccvalidator")
#'       ),
#'       file_summary_ui("summary")
#'     )
#'   )
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
file_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("file_to_summarize"),
      label = "Choose file to view",
      choices = ""
    ),
    tabsetPanel(
      tabPanel(
        "File Overview",
        plotOutput(ns("datafilevisdat"))
      ),
      tabPanel(
        "File Details",
        br(),
        reactable::reactableOutput(ns("data_details"))
      )
    )
  )
}

#' Server function for the file summary module
#'
#' Gives functionality to the file summary UI, populating the drop-down
#' menu with available files to choose from, and showing both an overview
#' and detailed summary of a chosen file.
#'
#' @export
#' @rdname file_summary
#' @param input the input variables from [shiny::callModule()]
#' @param output the output variables from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param file_data a reactive, named list of file data in data frames or
#'   tibbles
#' @return
file_summary_server <- function(input, output, session, file_data) {
  ## Placeholder for uploaded data that will be used to determine selectInput
  ## options for the data summary
  inputs <- reactiveVal(c())

  ## Update selectInput options for which data files to summarize
  observe({
    ## Find which ones are not null
    non_null <- vapply(
      file_data(),
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

  observeEvent(input$file_to_summarize, {
    if (input$file_to_summarize != "") {
      output$datafilevisdat <- renderPlot({
        visualize_data_types(file_data()[[input$file_to_summarize]])
      })
      file_summary <- data_summary(file_data()[[input$file_to_summarize]])
      output$data_details <- reactable::renderReactable({
        reactable::reactable(
          file_summary,
          highlight = TRUE,
          searchable = TRUE,
          resizable = TRUE,
          columns = list(
            variable = reactable::colDef(
              name = "Variable",
              width = 125
            ),
            type = reactable::colDef(
              name = "Type",
              width = 75
            ),
            missing = reactable::colDef(
              name = "Missing",
              maxWidth = 60
            ),
            complete = reactable::colDef(
              name = "Complete",
              maxWidth = 75
            ),
            n = reactable::colDef(
              name = "n",
              maxWidth = 40
            ),
            min = reactable::colDef(
              name = "Min",
              maxWidth = 40
            ),
            max = reactable::colDef(
              name = "Max",
              maxWidth = 45
            ),
            empty = reactable::colDef(
              name = "# Empty",
              maxWidth = 70
            ),
            n_unique = reactable::colDef(
              name = "# Unique",
              maxWidth = 70
            ),
            value_occurrence = reactable::colDef(
              name = "Value (# Occurrences)",
              cell = function(value) {
                if (nchar(value) > 40) {
                  return(glue::glue("{substr(value, 1, 40)}..."))
                } else {
                  return(value)
                }
              },
              details = function(index) {
                # browser()
                value <- file_summary[index, "value_occurrence"]
                if (nchar(value) > 40) {
                  return(htmltools::div(
                    shinydashboardPlus::boxPad(
                      br(),
                      glue::glue("{value[[1]]}"),
                      br(),
                      br(),
                      width = 12,
                      color = "gray")
                  )
                  )
                } else {
                  return(NULL)
                }
              }
            )
          )
        )
      })
    }
  })
}

#' @title Visualize data types
#'
#' @description Visualize the data class types, including
#' missing data, using the visdat and ggplot
#' packages.
#'
#' @param data Dataframe or tibble with file data.
visualize_data_types <- function(data) {
  if (!inherits(data, "tbl_df") && !inherits(data, "data.frame")) {
    return(NULL)
  }
  visdat::vis_dat(data) +
    ggplot2::theme(text = ggplot2::element_text(size = 16))
}

#' @title Get summary of data
#'
#' @description Get a summary of the data using the skimr
#' package, along with a custom function that appends an
#' extra column with a string showing each value for
#' a given variable and the number of times the value
#' appears. Returns a tibble with the data.
#'
#' @param data Dataframe or tibble with file data.
#' @return Tibble with summary information.
data_summary <- function(data) {
  if (!inherits(data, "tbl_df") && !inherits(data, "data.frame") 
      || nrow(data) == 0) {
    return(NULL)
  }
  data_sum <- tibble::as_tibble(skimr::skim_to_wide(data))
  # Cut out excess info from skimr results
  # Different classes result in different columns; get only ones that exist
  desired_cols <- c(
    "variable",
    "type",
    "missing",
    "complete",
    "n",
    "min",
    "max",
    "empty",
    "n_unique"
  )
  data_sum <- data_sum[, names(data_sum) %in% desired_cols]
  data_sum <- tibble::add_column(data_sum, value_occurrence = NA)
  for (var in data_sum$variable) {
    var_col <- which(names(data) == var)
    data_sum$value_occurrence[data_sum$variable == var] <-
      summarize_values(data[[var_col]])
  }
  data_sum
}

#' @title Summarize values present
#'
#' @description Get a list of values present and
#' the number of times each variable appeared.
#'
#' @param values The values to summarize in a list.
#' @return String with the form "value1 (2), value2 (4)",
#'   where the value is given with the number of
#'   occurrences in parenthesis.
summarize_values <- function(values) {
  if (is.null(values)) {
    return(NULL)
  }
  val_sum <- list()
  for (value in unique(values)) {
    x_appeared <- length(which(values == value))
    val_sum <- append(
      val_sum,
      glue::glue("{value} ({x_appeared})")
    )
  }
  val_sum_string <- glue::glue_collapse(val_sum, sep = ",  ")
  val_sum_string
}
