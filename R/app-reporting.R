#' Report results of validation checking
#'
#' Function to report results of a check. Shows an emoji (green check, yellow
#' warning, or red X), followed by the check message. If `verbose == TRUE`,
#' includes a details draw that shows the invalid data. Each result has an
#' informational button to the right that users can click to learn more about
#' what the check was looking for (in the check object this message is stored in
#' "behavior").
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
        emo::ji(emoji_prefix),
        result$message,
        ## Include details drawer for verbose == TRUE
        tags$details(show_details(result$data))
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
        emo::ji(emoji_prefix),
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

#' @param results A list of condition objects
#' @param ... Additional parameters passed to [report_result()]
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
#' @param x Content to be displayed
show_details <- function(x) {
  UseMethod("show_details", x)
}

#' @rdname show_details
show_details.default <- function(x) {
  paste0(x, collapse = ", ")
}

#' @rdname show_details
show_details.list <- function(x) {
  dat <- purrr::map2_dfr(names(x), x, function(y, z) {
    tibble::tibble(key = y, value = paste0(z, collapse = ", "))
  })
  renderTable(dat, colnames = FALSE)
}
