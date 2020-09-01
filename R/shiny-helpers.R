#' Reset shiny elements
#'
#' Reset one or more shiny elements. Note that this only resets the appearance
#' of the element, such as a fileInput, but does not reset the input value
#' of the element.
#'
#' @param ... The IDs of the elements to reset. If the element is from a module,
#' do not wrap with `ns()`.
reset_inputs <- function(...) {
  purrr::walk(c(...), function(x) {
    shinyjs::reset(x)
  })
}