#########################
####  Load packages  ####
#########################

## If running on server, load packages from local library
if (grepl("^ip-(.+)", Sys.info()["nodename"])) {
  .libPaths(new = "/home/kwoo/ShinyApps/dccvalidator-app/lib/")
}

library("DT")
library("shiny")
library("synapser")
library("purrr")
library("dccvalidator")

## Enable bookmarking
enableBookmarking(store = "url")

#####################
####  Functions  ####
#####################

## Function to generate messages when some IDs are mismatched
create_mismatched_id_message <- function(x, df1, df2, idtype) {
  imap(x, function(x, name) {
    new_name <- switch(
      name,
      "missing_from_x" = df1,
      "missing_from_y" = df2
    )
    if(length(na.omit(x)) > 0) {
      paste(
        "The following",
        idtype,
        "are missing from the",
        new_name,
        "file but present in the",
        setdiff(c(df1, df2), new_name),
        "file:",
        paste(x, collapse = ", ")
      )
    }
  })
}

## Create html output of messages about missing ids
report_mismatched_ids <- function(x, fallback_msg) {
  if (!all(map_lgl(x, is.null))) {
    result <- map(x, tags$p)
  } else {
    result <- p(fallback_msg)
  }
  result
}

## Report on completely missing ids
add_missing_ids <- function(x, column, file) {
  missing_ids <- which(is.na(column))
  if (length(missing_ids) > 0) {
    output <- p(
      paste(
        "The following rows of the",
        file,
        "metadata file are missing IDs:",
        paste(missing_ids, collapse = ", ")
      )
    )
    c(x, list(output))
  } else {
    x
  }
}

create_annotation_value_table <- function(output) {
  map2_dfr(
    output,
    names(output),
    function(x, y) {
      tibble::tibble(Key = y, Values = paste(x, collapse = ", "))
    }
  )
}
