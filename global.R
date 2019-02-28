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
