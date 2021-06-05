## Global Synapse module
synapse <- NULL

.onLoad <- function(libname, pkgname) {
  synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
}

set_staging_endpoints <- function(syn) {
  syn$setEndpoints(
    authEndpoint = synapse$client$STAGING_ENDPOINTS$authEndpoint,
    repoEndpoint = synapse$client$STAGING_ENDPOINTS$repoEndpoint,
    fileHandleEndpoint = synapse$client$STAGING_ENDPOINTS$fileHandleEndpoint,
    portalEndpoint = synapse$client$STAGING_ENDPOINTS$portalEndpoint
  )
}
