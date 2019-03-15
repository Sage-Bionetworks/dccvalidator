#####################################
####  Install and load packages  ####
#####################################

use_package <- function(p,
                        github,
                        repos = c(
                          CRAN = "https://cran.rstudio.com/",
                          Sage = "https://sage-bionetworks.github.io/ran"
                        ),
                        ...) {
  if (!missing(github)) {
    devtools::install_github(paste(github, p, sep = "/"), ...)
  }
  if (!p %in% installed.packages()[, "Package"]) {
    install.packages(p, repos = repos, dependencies = TRUE)
  }
  library(p, character.only = TRUE)
}

hosted_packages <- c("devtools", "shiny", "rmarkdown", "skimr", "synapser", "purrr")
lapply(hosted_packages, use_package)
use_package("dccvalidator", github = "Sage-Bionetworks")

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
        "metadata file:",
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
