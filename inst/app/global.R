#########################
####  Load packages  ####
#########################

library("DT")
library("ggplot2")
library("shiny")
library("shinydashboard")
library("skimr")
library("synapser")
library("purrr")
library("emo")
library("dccvalidator")
library("shinyBS")
library("tibble")
library("visdat")

#####################
####  Functions  ####
#####################

## Function to report results of a check. Shows an emoji (green check, yellow
## warning, or red X), followed by the check message. If verbose == TRUE,
## includes a details draw that shows the invalid data. Each result has an
## informational button to the right that users can click to learn more about
## what the check was looking for (in the check object this message is stored in
## "behavior").
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

## Wrapper to report_results() that reports a list of results
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

## Save uploaded files to Synapse
save_to_synapse <- function(input_file, parent, name = NULL) {
  file_to_upload <- File(
    input_file$datapath,
    parent = parent,
    name = name
  )
  synStore(file_to_upload)
}
