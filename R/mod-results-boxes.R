#' UI function for results boxes module
#'
#' This function outputs the html tags needed to
#' create UI for the successes, warnings, and
#' failures results boxes.
#'
#' @rdname results_boxes
#' @export
#' @param id The module id.
#' @return The html UI for the module.
#' @examples
#' library("shiny")
#' library("shinydashboard")
#'
#' server <- function(input, output) {
#'   # Create some sample results
#'   res <- list(
#'     check_pass(msg = "All good!", behavior = "Values should be >10"),
#'     check_fail(
#'       msg = "Some values are too small",
#'       behavior = "Values should be > 10",
#'       data = c(5.5, 1.3)
#'     )
#'   )
#'   # Show results in boxes
#'   callModule(results_boxes_server, "Validation Results", res)
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
#'       results_boxes_ui("Validation Results")
#'     )
#'   )
#' }
#' \dontrun{
#' shinyApp(ui, server)
#' }
results_boxes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      uiOutput(ns("successes")),
      solidHeader = TRUE,
      collapsible = TRUE,
      title = textOutput(ns("num_success")),
      status = "success",
      width = 12
    ),
    shinydashboard::box(
      uiOutput(ns("warnings")),
      solidHeader = TRUE,
      collapsible = TRUE,
      title = textOutput(ns("num_warn")),
      status = "warning",
      width = 12
    ),
    shinydashboard::box(
      uiOutput(ns("failures")),
      solidHeader = TRUE,
      collapsible = TRUE,
      title = textOutput(ns("num_fail")),
      status = "danger",
      width = 12
    )
  )
}

#' Server function for results boxes module
#'
#' This gives functionality to the results boxes module
#' UI, attaching titles and populating
#' the validation results.
#'
#' @rdname results_boxes
#' @export
#' @param input The input from [shiny::callModule()].
#' @param output The output from [shiny::callModule()].
#' @param session The session from [shiny::callModule()].
#' @param results List of the validation results. If `NULL`,
#'   box titles will be default strings (i.e. "Successess (0)");
#'   otherwise, the boxes will be populated with the
#'   results.
results_boxes_server <- function(input, output, session, results) {
  ## Initial titles for report boxes
  reporting_titles <- reactiveValues(
    success = "Successes (0)",
    warn = "Warnings (0)",
    fail = "Failures (0)"
  )
  output$num_success <- renderText(reporting_titles$success)
  output$num_warn <- renderText(reporting_titles$warn)
  output$num_fail <- renderText(reporting_titles$fail)

  if (!is.null(results)) {
    ## Populate validation report
    ## Successes box
    successes <- purrr::map_lgl(results, function(x) {
      inherits(x, "check_pass")
    })
    output$successes <- renderUI({
      report_results(results[successes], emoji_prefix = "\u2705")
    })
    reporting_titles$success <- glue::glue("Successes ({sum(successes)})")

    ## Warnings box
    warnings <- purrr::map_lgl(results, function(x) {
      inherits(x, "check_warn")
    })
    output$warnings <- renderUI({
      report_results(
        results[warnings],
        emoji_prefix = "\u26A0\uFE0F",
        verbose = TRUE
      )
    })
    reporting_titles$warn <- glue::glue("Warnings ({sum(warnings)})")

    ## Failures box
    failures <- purrr::map_lgl(results, function(x) {
      inherits(x, "check_fail")
    })
    output$failures <- renderUI({
      report_results(
        results[failures],
        emoji_prefix = "\u274c",
        verbose = TRUE
      )
    })
    reporting_titles$fail <- glue::glue("Failures ({sum(failures)})")
  }
}
