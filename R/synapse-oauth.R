## Global app variables for OAuth
APP_URL <- config::get("app_url")
CLIENT_ID <- Sys.getenv("client_id")
CLIENT_SECRET <- Sys.getenv("client_secret")
# Stop the app if the OAuth client ID or secret are missing
if (is.null(CLIENT_ID)) stop(".Renviron is missing client_id")
if (is.null(CLIENT_SECRET)) stop(".Renviron is missing client_secret")

app <- httr::oauth_app("dccvalidator",
                       key = CLIENT_ID,
                       secret = CLIENT_SECRET, 
                       redirect_uri = APP_URL)

## These are the user info details ('claims') requested from Synapse
CLAIMS <- list(
  family_name=NULL, 
  given_name=NULL,
  userid=NULL,
  is_certified=NULL
)
CLAIMS_PARAM <- jsonlite::toJSON(list(id_token = CLAIMS, userinfo = CLAIMS))
api <- httr::oauth_endpoint(
  authorize=paste0("https://signin.synapse.org?claims=", CLAIMS_PARAM),
  access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

# The 'openid' scope is required by the protocol for retrieving user information.
SCOPE <- "openid view download modify"

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

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

#' OAuth UI pops in before the main app_ui loads
#' @param request Shiny request object
oauth_ui <- function(request) {
  ## If this is the first time signing in, will need to do OAuth process
  ## which will redirect back here
  ## At that point, the token should be available and the main app_ui should
  ## be loaded
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