.First <- function() {
  options(
    repos = c(
      CRAN = "https://cran.rstudio.com/"
    )
  )
}

if (!identical(Sys.getenv("CI"), "true")) { # don't run on travis
  source("renv/activate.R")
  renv::settings$ignored.packages(c("pkgdown"), persist = TRUE)
}
