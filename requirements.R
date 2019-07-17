#####################################
####  Install required packages  ####
#####################################

## Not run by the shiny app, this can be used to install packages to a local
## library within the app directory. If you don't install and load the packages
## from a local library, the app will install and load them from the server-wide
## library, which may not be ideal.

## Usage:
## $ Rscript requirements.R

## Source rprofile so this script can be run with rscript and still know about
## custom lib path, CRAN mirrors, etc.
source(".Rprofile")

## Install packages
to_install <- c(
  "DT",
  "ggplot2",
  "purrr",
  "remotes",
  "shinyBS",
  "shinydashboard",
  "skimr",
  "synapser",
  "tibble",
  "visdat"
)
install.packages(to_install)

## Install packages from GitHub
remotes::install_github("hadley/emo")
remotes::install_github("Sage-Bionetworks/dccvalidator")
