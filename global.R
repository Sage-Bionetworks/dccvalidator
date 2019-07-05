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

## Enable bookmarking
enableBookmarking(store = "url")

#####################
####  Functions  ####
#####################

report_result <- function(result, emoji_prefix = NULL, verbose = FALSE) {
  if (isTRUE(verbose)) {
    div(
      p(
        emo::ji(emoji_prefix),
        result$message,
        tags$details(paste0(result$data, collapse = ", "))
      )
    )
  } else {
    p(emo::ji(emoji_prefix), result$message)
  }
}

report_results <- function(results, ...) {
  map(results, report_result, ...)
}
