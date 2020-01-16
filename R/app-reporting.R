#' Report results of validation checking
#'
#' Function to report results of a check. Shows an emoji (green check, yellow
#' warning, or red X), followed by the check message. If `verbose == TRUE`,
#' includes a details draw that shows the invalid data. Each result has an
#' informational button to the right that users can click to learn more about
#' what the check was looking for (in the check object this message is stored in
#' "behavior").
#'
#' @noRd
#' @param result An object of type `check_pass`, `check_warn`, or `check_fail`
#'   to report
#' @param emoji_prefix Emoji prefix to accompany reported result
#' @param verbose If `TRUE`, provides additional data about the check result
#'   (accessed from the `$data` slot of the check object)
#' @return A set of HTML elements showing the result of the check
#' @importFrom shinyBS popify
report_result <- function(result, emoji_prefix = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    div(
      class = "result",
      div(
        class = "wide",
        emoji_prefix,
        result$message,
        ## Include details drawer for verbose == TRUE
        tags$details(
          tags$summary("Show details"),
          show_details(result$data)
        )
      ),
      popify(
        tags$a(icon(name = "question-circle"), href = "#"),
        "Information",
        result$behavior,
        placement = "left",
        trigger = "click"
      )
    )
  } else {
    div(
      class = "result",
      div(
        class = "wide",
        emoji_prefix,
        result$message
      ),
      popify(
        tags$a(icon(name = "question-circle"), href = "#"),
        "Information",
        result$behavior,
        placement = "left",
        trigger = "click"
      )
    )
  }
}

#' @noRd
#' @param results A list of condition objects
#' @param ... Additional parameters passed to [report_result()]
#' @return A list of HTML elements showing the results of multiple checks
#' @rdname report_result
report_results <- function(results, ...) {
  purrr::map(results, report_result, ...)
}

#' Show additional details in a drawer
#'
#' S3 method to show details from a condition object. If the data included in
#' the object is a vector, it prints the elements separated by commas. If the
#' data is a list, it creates an HTML table with two columns: one containing the
#' list elements' names, and the other containing comma-separated values.
#'
#' @noRd
#' @param x Content to be displayed
#' @return Additional information to be displayed within the details drawer. If
#'   `x` is an atomic vector, each element will be displayed separated by a
#'   comma. If `x` is a list, it will be converted to a table for display.
show_details <- function(x) {
  UseMethod("show_details", x)
}

#' @noRd
#' @rdname show_details
show_details.default <- function(x) {
  paste0(x, collapse = ", ")
}

#' @noRd
#' @rdname show_details
show_details.list <- function(x) {
  dat <- purrr::map_dfr(x, function(x) {
    tibble::tibble(value = paste0(x, collapse = ", "))
  }, .id = "key")
  renderTable(dat, colnames = FALSE)
}


#' Create a modal dialog if user is not in required team(s) or certified
#'
#' Takes the output from [check_team_membership()] and [check_certified_user()].
#' If the user is not in the required teams or certified, creates a modal dialog
#' indicating which teams they need to belong to and how to request access.
#'
#' @noRd
#' @inheritParams get_synapse_table
#' @param membership Output from [check_team_membership()]
#' @param certified Output from [check_certified_user()]
#' @return If user is not certified or in the required teams, a modal dialog
#'   describing which requirements are not met.
report_unsatisfied_requirements <- function(membership, certified, syn) {
  member_message <- tagList()
  certified_message <- tagList()
  if (inherits(membership, "check_fail")) {
    team_links <- glue::glue_collapse(
      purrr::map_chr(
        membership$data,
        function(x) {
          glue::glue("<a href=\"https://www.synapse.org/#!Team:{x}\">https://www.synapse.org/#!Team:{x}</a>") # nolint
        }
      ),
      sep = "<br>"
    )
    missing_teams <- glue::glue_collapse(
      purrr::map_chr(membership$data, function(x) syn$getTeam(x)$name),
      sep = ", "
    )
    member_message <- tagList(
      p(tags$b(membership$message)),
      p(membership$behavior),
      p("You can request to be added at:"),
      HTML(team_links)
    )
  }
  if (inherits(certified, "check_fail")) {
    certified_message <- tagList(
      p(tags$b(certified$message)),
      HTML(certified$behavior)
    )
  }

  if (inherits(membership, "check_fail") | inherits(certified, "check_fail")) {
    showModal(
      modalDialog(
        title = "Synapse requirements not met",
        member_message,
        br(),
        br(),
        certified_message
      )
    )
  }
}
