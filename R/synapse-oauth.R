if (interactive()) {
  # testing url
  options(shiny.port = 8100)
  APP_URL <- "http://127.0.0.1:8100"
} else {
  # TODO: This has to be updated per application
  APP_URL <- "https://shinypro.synapse.org/users/nkauer/dccvalidator-test/"
}

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth code is present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
}

client_id <- Sys.getenv("client_id")
client_secret <- Sys.getenv("client_secret")
if (is.null(client_id)) stop(".Renviron is missing client_id")
if (is.null(client_secret)) stop(".Renviron is missing client_secret")

app <- httr::oauth_app("dccvalidator",
                 key = client_id,
                 secret = client_secret, 
                 redirect_uri = APP_URL)

# These are the user info details ('claims') requested from Synapse:
claims <- list(
  family_name=NULL, 
  given_name=NULL,
  email=NULL,
  email_verified=NULL,
  userid=NULL,
  orcid=NULL,
  is_certified=NULL,
  is_validated=NULL,
  validated_given_name=NULL,
  validated_family_name=NULL,
  validated_location=NULL,
  validated_email=NULL,
  validated_company=NULL,
  validated_at=NULL,
  validated_orcid=NULL,
  company=NULL
)

claimsParam <- jsonlite::toJSON(list(id_token = claims, userinfo = claims))
api <- httr::oauth_endpoint(
  authorize=paste0("https://signin.synapse.org?claims=", claimsParam),
  access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
)

# The 'openid' scope is required by the protocol for retrieving user information.
scope <- "openid view download modify"

oauth_process <- function(params) {
  # if (!has_auth_code(params)) {
  #   return()
  # }
  redirect_url <- paste0(api$access, '?', 'redirect_uri=',
                         APP_URL, '&grant_type=',
                         'authorization_code' ,'&code=', params$code)
  # get the access_token and userinfo token
  req <- httr::POST(redirect_url,
              encode = "form",
              body = '',
              httr::authenticate(app$key, app$secret, type = "basic"),
              config = list())
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(req, task = "get an access token")
  token_response <- httr::content(req, type = NULL)
  access_token <- token_response$access_token
  access_token
}