## Try to get Synapse client object
attempt_instantiate <- function() {
  if (reticulate::py_module_available("synapseclient")) {
    return(synapse$Synapse())
  } else {
    return(NULL)
  }
}

#' @title Attempt to log into Synapse
#'
#' @description Attempt to log into Synapse. Will first try using authentication
#' credentials written to a .synapseConfig file. If that fails, will try using
#' any credentials passed to the function. Will return `NULL` if not all
#' attempts failed.
#'
#' @export
#' @param syn Synapse client object
#' @param ... Synapse credentials, such as `authToken` or `email` with a
#' `password` or `apiKey`.
attempt_login <- function(syn, ...) {
  is_logged_in <- FALSE
  ## Try logging in with .synapseConfig
  tryCatch(
    {
      syn$login()
      is_logged_in <- TRUE
    },
    error = function(e) {
      stop("There was a problem logging in using stored credentials.")
    }
  )
  ## If failed to login, try using credentials provided
  if (!is_logged_in) {
    tryCatch(
      {
        syn$login(...)
      },
      error = function(e) {
        stop("There was a problem logging in using given credentials.")
      }
    )
  }
}

#' @title Check if logged in as user
#'
#' @description Check if logged into Synapse as a non-anonymous user.
#'
#' @param syn Synapse client object.
#' @return FALSE if not logged in at all or if logged in anonymously, else TRUE.
logged_in <- function(syn) {
  stopifnot(inherits(syn, "synapseclient.client.Synapse"))
  if (is.null(syn) || is.null(syn$username) || (syn$username == "anonymous")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

## Save uploaded files to Synapse
save_to_synapse <- function(input_file,
                            parent,
                            annotations = NULL,
                            synapseclient,
                            syn) {
  if (!is_name_valid(input_file$name)) {
    stop("Please check that file names only contain: letters, numbers, spaces, underscores, hyphens, periods, plus signs, and parentheses.") # nolint
  }
  file_to_upload <- synapseclient$File(
    input_file$datapath,
    parent = parent,
    name = input_file$name,
    annotations = annotations
  )
  syn$store(file_to_upload)
}

#' @title Set Synapse Staging Endpoints
#'
#' @description By default, the Synapse Python client points to the production
#' endpoints. This function will point the client to the staging endpoints for
#' testing.
#'
#' @export
#' @param syn Synapse client object
set_staging_endpoints <- function(syn) {
  syn$setEndpoints(
    authEndpoint = synapse$client$STAGING_ENDPOINTS$authEndpoint,
    repoEndpoint = synapse$client$STAGING_ENDPOINTS$repoEndpoint,
    fileHandleEndpoint = synapse$client$STAGING_ENDPOINTS$fileHandleEndpoint,
    portalEndpoint = synapse$client$STAGING_ENDPOINTS$portalEndpoint
  )
}
