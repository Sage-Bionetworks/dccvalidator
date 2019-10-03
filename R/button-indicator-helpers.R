# Author: Dean Attali
# From https://github.com/daattali/advanced-shiny/blob/master/busy-indicator/helpers.R 

with_busy_indicator_css <- "
.btn-loading-container {
margin-left: 10px;
font-size: 1.2em;
}
.btn-done-indicator {
color: green;
}
.btn-err {
margin-top: 10px;
color: red;
}
"

with_busy_indicator_ui <- function(button) {
  id <- button[["attribs"]][["id"]]
  div(
    shinyjs::useShinyjs(),
    singleton(tags$head(
      tags$style(with_busy_indicator_css)
    )),
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
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
with_busy_indicator_server <- function(button_id, expr) {
  # UX stuff: show the "busy" message, hide the other messages,
  # disable the button
  loading_el <- sprintf("[data-for-btn=%s] .btn-loading-indicator", button_id)
  done_el <- sprintf("[data-for-btn=%s] .btn-done-indicator", button_id)
  err_el <- sprintf("[data-for-btn=%s] .btn-err", button_id)
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
  tryCatch({
    value <- expr
    shinyjs::show(selector = done_el)
    shinyjs::delay(2000, shinyjs::hide(selector = done_el,
                                       anim = TRUE,
                                       animType = "fade",
                                       time = 0.5))
    value
  },
  error = function(err) {
    error_func(err, button_id)
  })
}

# When an error happens after a button click, show the error
error_func <- function(err, button_id) {
  err_el <- sprintf("[data-for-btn=%s] .btn-err", button_id)
  err_el_msg <- sprintf("[data-for-btn=%s] .btn-err-msg", button_id)
  err_message <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = err_message, selector = err_el_msg)
  shinyjs::show(selector = err_el, anim = TRUE, animType = "fade")
}
