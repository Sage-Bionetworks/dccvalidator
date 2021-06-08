#' @title App OAuth startup switcheroo
#'
#' @description Handles logic of launching the main [app_ui()] or the
#' [mod_oauth_ui()] based on whether the app is running locally (main UI) or not
#' (OAuth UI). If using OAuth, this function assumes a config file, with OAuth
#' options `claims`, `scope`, and `app_url`, accessible with
#' [`get_golem_config("oauth")`] and a .Renviron stored locally to the app with
#' `Client_Name`, `Client_ID`, and `Client_Secret`.
#'
#' @param request Shiny request object
app_ui_startup_switch <- function(request) {
  # if(interactive()) {
  #   ## Running locally; skip OAuth
  #   app_ui(request)
  # } else {
    mod_synapse_oauth_ui(id = "oauth")
  # }
}

#' @title Synapse Oauth Module
#'
#' @description Create the Synapse OAuth component of a Shiny app. Launches the
#' OAuth UI. If this is the first time signing in, will need to do OAuth the
#' process. The OAuth process will redirect back here and an authorization code
#' should be in the URL parameters. If this code is available, lauch the main
#' app_ui. Note that the UI function does not accept an `id`, but rather a
#' the Shiny `request` object. This function should be called from startup or
#' called from the main UI, which receives the `request` and can pass it along.
#'
#' @import shiny
#' @param request Shiny request object.
#' @param client_name Synapse OAuth client name.
#' @param client_id Synapse OAuth client ID.
#' @param client_secret Synapse OAuth client secret.
#' @param app_url Application URL to redirect back to after OAuth process.
#' @param claims List of items to request access to. Defaults to `family_name`,
#' `given_name`, `userid`, and `is_certified`. See ##TODO
#' @param scope Space-seperated access scope. Defaults to
#' `openid view download modify`. See ##TODO
#' @examples
#' \dontrun{
#' ## TODO
#' }
mod_synapse_oauth_ui <- function(id, request,
                                 client_name, client_id, client_secret, app_url,
                                 claims = list(
                                   family_name=NULL, 
                                   given_name=NULL,
                                   userid=NULL,
                                   is_certified=NULL
                                 ),
                                 scope = "openid view download modify",
                                 app_ui = app_ui(request)) {
  ns <- NS(id)
  # ## Set up client app
  # app <- httr::oauth_app(
  #   client_name,
  #   key = client_id,
  #   secret = client_secret, 
  #   redirect_uri = app_url
  # )
  # ## Set up client api
  # claims_param <- jsonlite::toJSON(list(id_token = claims, userinfo = claims))
  # api <- httr::oauth_endpoint(
  #   authorize=paste0("https://signin.synapse.org?claims=", claims_param),
  #   access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
  # )
  # 
  # ## If access token not available, launch OAuth, else launch main app UI
  # if (!has_auth_code(parseQueryString(request$QUERY_STRING))) {
  #   authorization_url = httr::oauth2.0_authorize_url(api, app, scope = scope)
  #   return(
  #     tags$script(
  #       HTML(sprintf("location.replace(\"%s\");", authorization_url))
  #     )
  #   )
  # } else {
  #   app_ui
  # }
  uiOutput(ns("ui"))
}

#' @title 
#' 
mod_synapse_oauth_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ## Set up client app
      app <- httr::oauth_app(
        client_name,
        key = client_id,
        secret = client_secret, 
        redirect_uri = app_url
      )
      ## Set up client api
      claims_param <- jsonlite::toJSON(
        list(id_token = claims, userinfo = claims)
      )
      api <- httr::oauth_endpoint(
        authorize=paste0("https://signin.synapse.org?claims=", claims_param),
        access="https://repo-prod.prod.sagebase.org/auth/v1/oauth2/token"
      )
      ## Set up client authorization url
      authorization_url = httr::oauth2.0_authorize_url(api, app, scope)
  
      url_params <- parseQueryString(isolate(session$clientData$url_search))
      ## If access token not available, launch OAuth, else launch main app UI
      if (!has_auth_code(url_params)) {
        output$ui <- renderUI(
          tags$script(
            HTML(sprintf("location.replace(\"%s\");", authorization_url))
          )
        )
      } else {
        output$ui <- renderUI(app_ui)
      }  
    }
  )
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
