## Global Synapse module
synapse <- NULL
## Global OAuth client and url
app <- NULL
APP_URL <- NULL

# ## Global app variables for OAuth
# if (interactive()) {
#   # testing url
#   options(shiny.port = 8100)
#   APP_URL <- "http://127.0.0.1:8100"
# } else {
#   APP_URL <- config::get("app_url")
# }
## These are the user info details ('claims') requested from Synapse
CLAIMS <- list(
  family_name=NULL, 
  given_name=NULL,
  userid=NULL,
  is_certified=NULL
)
CLAIMS_PARAM <- jsonlite::toJSON(list(id_token = CLAIMS, userinfo = CLAIMS))
# The 'openid' scope is required by the protocol for retrieving user information.
SCOPE <- "openid view download modify"

## Client specifiv variables
CLIENT_ID <- Sys.getenv("client_id")
CLIENT_SECRET <- Sys.getenv("client_secret")
# Stop the app if the OAuth client ID or secret are missing
if (is.null(CLIENT_ID)) stop(".Renviron is missing client_id")
if (is.null(CLIENT_SECRET)) stop(".Renviron is missing client_secret")

## Prep the OAuth app and api endpoint
# app <- httr::oauth_app("dccvalidator",
#                        key = CLIENT_ID,
#                        secret = CLIENT_SECRET, 
#                        redirect_uri = APP_URL)
api <- httr::oauth_endpoint(
  authorize=paste0("https://signin.synapse.org?claims=", CLAIMS_PARAM),
  access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

## Bring in the Synapse Python Client
.onLoad <- function(libname, pkgname) {
  synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
}

## Prep OAuth client
prep_for_oauth <- function(app_url) {
  APP_URL <<- app_url
  app <<- httr::oauth_app("dccvalidator",
                         key = CLIENT_ID,
                         secret = CLIENT_SECRET, 
                         redirect_uri = APP_URL)
}

#' @title Has authorization code
#'
#' @description Check if the authorization code is in the parsed URL parameters.
#'
#' @param params List of parsed URL parameters.
has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

#' @title Do the OAuth process
#'
#' @description Go through the OAuth process to get an access token for log in.
#'
#' @export
#' @inheritParams has_auth_code
#' @return Nothing if the params have an authorization code already present.
#' Otherwise, kicks off the OAuth process and returns an access token for
#' log in. 
oauth_process <- function(params) {
  if (!has_auth_code(params)) {
    return()
  }
  redirect_url <- glue::glue(
    "{api$access}?redirect_uri={APP_URL}&grant_type=authorization_code",
    "&code={params$code}"
  )
  # get the access_token and userinfo token
  req <- httr::POST(
    redirect_url,
    encode = "form",
    body = '',
    httr::authenticate(app$key, app$secret, type = "basic"),
    config = list()
  )
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
  access_token
}

#' @title OAuth UI
#'
#' @description Launches the OAuth UI. If this is the first time signing in,
#' will need to do OAuth the process. The OAuth process will redirect back here
#' and an authorization code should be in the URL parameters. If this code is
#' available, lauch the main app_ui.
#'
#' @export
#' @param request Shiny request object
oauth_ui <- function(request) {
  prep_for_oauth(config::get("app_url"))
  if (!has_auth_code(parseQueryString(request$QUERY_STRING))) {
    authorization_url = httr::oauth2.0_authorize_url(api, app, scope = SCOPE)
    return(
      tags$script(
        HTML(sprintf("location.replace(\"%s\");", authorization_url))
      )
    )
  } else {
    app_ui(request)
  }
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
