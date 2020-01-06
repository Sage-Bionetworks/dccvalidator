synapse <- NULL

.onLoad <- function(libname, pkgname) {
  synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
}
