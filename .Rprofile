.First <- function() {
  options(
    repos = c(
      CRAN = "https://cran.rstudio.com/",
      Sage = "http://ran.synapse.org"
    )
  )
}

## If running on server, load packages from local library
if (grepl("^ip-(.+)", Sys.info()["nodename"])) {
  .libPaths(new = "/home/kwoo/ShinyApps/dccvalidator-app/lib/")
}
