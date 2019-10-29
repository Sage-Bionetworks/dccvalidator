#' Show busy indicator
#'
#' These functions add button feedback features including:
#' disabling the button while processing requested function,
#' showing a spinning wheel while processing requested function,
#' displaying a green checkmark showing success upon completion,
#' or displaying an error message if the function requested failed.
#'
#' Author: Dean Attali
#' From https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R #nolint
#'
#' Hint for making this work with modules by mmoise in PR#11

#' Wrap the button in this function to attach visual features
#'
#' @keywords internal
#' @param button a shiny actionButton
with_busy_indicator_ui <- function(button) {
  id <- button[["attribs"]][["id"]]
  div(
    shinyjs::useShinyjs(),
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      shinyjs::hidden(
        icon("spinner", class = "btn-loading-indicator fa-spin"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    shinyjs::hidden(
      div(
        class = "btn-err",
        div(
          icon("exclamation-circle"),
          tags$b("Error: "),
          span(class = "btn-err-msg")
        )
      )
    )
  )
}

#' When a button is clicked, call this function from the server.
#' This will execute the expression desired and signal the visual
#' feedback for the button. If the expression fails, the error
#' message displayed is from the error that was thrown.
#'
#' @keywords internal
#' @rdname with_busy_indicator_ui
#' @param button_id id of shiny actionButton
#' @param expr the code to run when the button is clicked
with_busy_indicator_server <- function(button_id, expr) {
  # UX stuff: show the "busy" message, hide the other messages,
  # disable the button
  # Need to get session in order for button to indicate correctly
  session <- getDefaultReactiveDomain()
  loading_el <- sprintf(
    "[data-for-btn=%s] .btn-loading-indicator",
    session$ns(button_id)
  )
  done_el <- sprintf(
    "[data-for-btn=%s] .btn-done-indicator",
    session$ns(button_id)
  )
  err_el <- sprintf(
    "[data-for-btn=%s] .btn-err",
    session$ns(button_id)
  )
  shinyjs::disable(button_id)
  shinyjs::show(selector = loading_el)
  shinyjs::hide(selector = done_el)
  shinyjs::hide(selector = err_el)
  on.exit({
    shinyjs::enable(button_id)
    shinyjs::hide(selector = loading_el)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch(
    # nolint start
    {
      value <- expr
      shinyjs::show(selector = done_el)
      shinyjs::delay(2000, shinyjs::hide(
        selector = done_el,
        anim = TRUE,
        animType = "fade",
        time = 0.5
      ))
      value
    },
    # nolint end
    error = function(err) {
      error_func(err, button_id)
    }
  )
}

#' Error message helper.
#'
#' @noRd
#' @param err the error
#' @param button_id id for the actionButton
error_func <- function(err, button_id) {
  # Need to get session first
  session <- getDefaultReactiveDomain()
  err_el <- sprintf(
    "[data-for-btn=%s] .btn-err",
    session$ns(button_id)
  )
  err_el_msg <- sprintf(
    "[data-for-btn=%s] .btn-err-msg",
    session$ns(button_id)
  )
  err_message <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = err_message, selector = err_el_msg)
  shinyjs::show(selector = err_el, anim = TRUE, animType = "fade")
}
