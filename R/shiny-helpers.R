#' Reset shiny elements
#'
#' Reset one or more shiny elements. Note that this only resets the appearance
#' of the element, such as a fileInput, but does not reset the input value
#' of the element.
#'
#' @noRd
#' @param ... The IDs of the elements to reset. If the element is from a module,
#' do not wrap with `ns()`.
reset_inputs <- function(...) {
  purrr::walk(c(...), function(x) {
    shinyjs::reset(x)
  })
}

#' @title Wrapper for modals
#'
#' @description This wraps the functions for showing a modal with a message.
#'
#' @param title Title text for modal
#' @param message The message to show in the modal
show_modal <- function(title, message) {
  showModal(
    modalDialog(
      title = title,
      HTML(message)
    )
  )
}
