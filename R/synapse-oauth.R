## Global Synapse module
synapse <- NULL

## Bring in the Synapse Python Client
.onLoad <- function(libname, pkgname) {
  synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
}

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
#' @return TRUE if logged in, else `NULL`
attempt_login <- function(syn, ...) {
  is_logged_in <- FALSE
  ## Try logging in with .synapseConfig
  try(
    {
      syn$login()
      is_logged_in <- TRUE
    },
    silent = TRUE
  )
  ## If failed to login, try using credentials provided
  if(!is_logged_in) {
    try(
      {
        syn$login(...)
        is_logged_in <- TRUE
      }
    )
  }
  ## Return TRUE if logged in, else NULL
  ifelse(is_logged_in, is_logged_in, NULL)
}

#' @title Check if logged in as user
#'
#' @description Check if logged into Synapse as a non-anonymous user.
#'
#' @param syn Synapse client object.
#' @return FALSE if not logged in at all or if logged in anonymously, else TRUE.
logged_in <- function(syn) {
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

#' #' @title Do the OAuth process
#' #'
#' #' @description Go through the OAuth process to get an access token for log in.
#' #'
#' #' @export
#' #' @inheritParams has_auth_code
#' #' @return Nothing if the params have an authorization code already present.
#' #' Otherwise, kicks off the OAuth process and returns an access token for
#' #' log in. 
#' oauth_process <- function(params) {
#'   if (!has_auth_code(params)) {
#'     return()
#'   }
#'   redirect_url <- glue::glue(
#'     "{api$access}?redirect_uri={APP_URL}&grant_type=authorization_code",
#'     "&code={params$code}"
#'   )
#'   # get the access_token and userinfo token
#'   req <- httr::POST(
#'     redirect_url,
#'     encode = "form",
#'     body = '',
#'     httr::authenticate(app$key, app$secret, type = "basic"),
#'     config = list()
#'   )
#'   # Stop the code if anything other than 2XX status code is returned
#'   httr::stop_for_status(req, task = "get an access token")
#'   token_response <- httr::content(req, type = NULL)
#'   access_token <- token_response$access_token
#'   access_token
#' }

#' #' @title OAuth UI
#' #'
#' #' @description Launches the OAuth UI. If this is the first time signing in,
#' #' will need to do OAuth the process. The OAuth process will redirect back here
#' #' and an authorization code should be in the URL parameters. If this code is
#' #' available, lauch the main app_ui.
#' #'
#' #' @export
#' #' @param request Shiny request object
#' oauth_ui <- function(request) {
#'   prep_for_oauth(config::get("app_url"))
#'   if (!has_auth_code(parseQueryString(request$QUERY_STRING))) {
#'     authorization_url = httr::oauth2.0_authorize_url(api, app, scope = SCOPE)
#'     return(
#'       tags$script(
#'         HTML(sprintf("location.replace(\"%s\");", authorization_url))
#'       )
#'     )
#'   } else {
#'     app_ui(request)
#'   }
#' }

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
