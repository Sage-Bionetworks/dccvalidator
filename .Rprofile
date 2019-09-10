.First <- function() {
  options(
    repos = c(
      CRAN = "https://cran.rstudio.com/",
      Sage = "http://ran.synapse.org"
    )
  )
}

if (!identical(Sys.getenv("TRAVIS"), "true")) { # don't run on travis
  source("renv/activate.R")
}
