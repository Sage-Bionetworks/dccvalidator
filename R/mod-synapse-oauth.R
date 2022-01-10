## Global Synapse module
synapse <- NULL
app <- NULL
api <- NULL
scope <- NULL
app_url <- NULL
claims_param <- NULL
authorization_url <- NULL

#' @title Set up Synapse and OAuth variables
#'
#' @description Bring in the Synapse Python Client and set up global variables
#' for OAuth at startup. Requires global variables initialized to NULL: synapse,
#' app, api, scope, app_url, claims_param, authorization_url.
#'
#' @export
#' @param libname default R .onLoad() parameter
#' @param pkgname default R .onLoad() parameter
.onLoad <- function(libname, pkgname) {
  synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
  #automatically check if the synapseclient version is larger than 2.3.1,
  #otherwise, print the error message in log file
  syn_version <- synapse["__version__"]
  stopifnot(compareVersion(syn_version, "2.3.1") >= 0)
  if (!interactive()) {
    setup_global_oauth_vars(
      app_url = get_golem_config("app_url"),
      client_name = Sys.getenv("client_name"),
      client_id = Sys.getenv("client_id"),
      client_secret = Sys.getenv("client_secret")
    )
  }
}

#' @title Synapse Oauth Module
#'
#' @description Create the Synapse OAuth component of a Shiny app.
#'
#' If this is
#' the first time signing in, will need to do OAuth the process. The OAuth
#' process will redirect back here and an authorization code
#' should be in the URL parameters. If this code is available, lauch the main
#' app_ui. This function should be called from startup
#' called from the UI function specified when creating the application object
#' (i.e. the function specified in the `ui` parameter here
#' `shinyApp(ui = app_ui, server = app_server)`), which receives the `request`
#' object and can pass it along.
#'
#' IMPORTANT: this module assumes the following
#' global variables are available and valid: app, api, authorization_url,
#' app_url, claims_params, scope. See \code{\link{setup_global_oauth_vars}}.
#'
#' @export
#' @import shiny
#' @param id The module id.
#' @param request Shiny request object.
#' @param main_ui UI function to redirect to when OAuth is done. Defaults to
#' \code{\link{mod_main_ui}}.
#' @param main_ui_id The module id to use when launching `main_ui`. Defaults
#' to "main".
#' @examples
#' \dontrun{
#' library("dccvalidator")
#' app_ui <- function(request) {
#'   mod_synapse_oauth_ui(id = "oauth", request = request)
#' }
#' app_server <- function(input, output, session) {
#'   synapse <- reticulate::import("synapseclient")
#'   syn <- synapse$Synapse()
#'   syn <- mod_synapse_oauth_server(
#'     id = "oauth",
#'     syn = syn
#'   )
#'   shiny::req(inherits(syn, "synapseclient.client.Synapse"), logged_in(syn))
#' }
#' run_app()
#' }
mod_synapse_oauth_ui <- function(id, request,
                                 main_ui = mod_main_ui, main_ui_id = "main") {
  ns <- NS(id)

  # If access token not available, launch OAuth, else launch main app UI
  if (!has_auth_code(parseQueryString(request$QUERY_STRING))) {
    # authorization_url = httr::oauth2.0_authorize_url(api, app, scope = scope)
    return(
      tags$script(
        HTML(sprintf("location.replace(\"%s\");", authorization_url))
      )
    )
  } else {
    main_ui(main_ui_id)
  }
}

#' @title Synapse OAuth server
#'
#' @export
#' @rdname mod_synapse_oauth_ui
#' @import shiny
#' @param id The module id.
#' @param syn Synapse client object
mod_synapse_oauth_server <- function(id, syn) {
  moduleServer(
    id,
    function(input, output, session) {
      url_params <- parseQueryString(isolate(session$clientData$url_search))
      if (has_auth_code(url_params)) {
        accessToken <- oauth_process(params = url_params)
        if (inherits(accessToken, "character")) {
          attempt_login(syn, authToken = accessToken)
        }
      }
      return(syn)
    }
  )
}

#' @title Sets up global OAuth variables
#'
#' @description Sets up the global OAuth variables needed for all users. These
#' must be initiated in the global environment for this to function correctly.
#'
#' @export
#' @param app_url Application URL to redirect back to after OAuth process.
#' @param client_name Synapse OAuth client name.
#' @param client_id Synapse OAuth client ID.
#' @param client_secret Synapse OAuth client secret.
#' @param claims List of items to request access to. Defaults to `family_name`,
#' `given_name`, `userid`, and `is_certified`. See ##TODO
#' @param scope Space-seperated access scope. Defaults to
#' `openid view download modify`. See ##TODO
setup_global_oauth_vars <- function(app_url,
                                    client_name, client_id, client_secret,
                                    claims = list(
                                      family_name = NULL,
                                      given_name = NULL,
                                      userid = NULL,
                                      is_certified = NULL
                                    ),
                                    scope = "openid view download modify") {
  scope <<- scope
  app_url <<- app_url
  claims_param <<- jsonlite::toJSON(
    list(id_token = claims, userinfo = claims)
  )
  app <<- httr::oauth_app(
    client_name,
    key = client_id,
    secret = client_secret,
    redirect_uri = app_url
  )
  api <<- httr::oauth_endpoint(
    authorize = paste0("https://signin.synapse.org?claims=", claims_param),
    access = "https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
  )
  authorization_url <<- httr::oauth2.0_authorize_url(api, app, scope)
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
  # params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }
  redirect_url <- glue::glue(
    "{api$access}?redirect_uri={app_url}&grant_type=authorization_code",
    "&code={params$code}"
  )
  # get the access_token and userinfo token
  token_request <- httr::POST(
    redirect_url,
    encode = "form",
    body = "",
    httr::authenticate(app$key, app$secret, type = "basic")
  )
  # Stop the code if anything other than 2XX status code is returned
  httr::stop_for_status(token_request, task = "get an access token")
  token_response <- httr::content(token_request, type = NULL)
  access_token <- token_response$access_token
  access_token
}
