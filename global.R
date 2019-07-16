#########################
####  Load packages  ####
#########################

library("DT")
library("shiny")
library("shinydashboard")
library("synapser")
library("purrr")
library("emo")
library("dccvalidator")
library("shinyBS")
library("tibble")

#####################
####  Functions  ####
#####################

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

report_results <- function(results, ...) {
  map(results, report_result, ...)
}

## Methods for showing additional details about the errors
show_details <- function(x) {
  UseMethod("show_details", x)
}

## By default, just show the values separated by commas
show_details.default <- function(x) {
  paste0(x, collapse = ", ")
}

## If data is a list, convert to a table where there is one column for the names
## of the elements, and a second column for the comma-separated values
show_details.list <- function(x) {
  dat <- map2_dfr(names(x), x, function(y, z) {
    tibble(key = y, value = paste0(z, collapse = ", "))
  })
  renderTable(dat, colnames = FALSE)
}
