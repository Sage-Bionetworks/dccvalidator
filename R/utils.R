## Check if we are running on CI
on_ci <- function() {
  if (identical(Sys.getenv("CI"), "true")) {
    return(TRUE)
    # nocov start
  } else {
    return(FALSE)
  }
  # nocov end
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

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

## Count unique values
count_unique_values <- function(...) {
  sum(!is.na(unique(c(...))))
}

## Do whole login process
## Making a whole function so it can be more easily mocked for other functions
full_login_process <- function(...) {
  # Import synapseclient and login
  synapse <<- reticulate::import("synapseclient")
  syn <- attempt_instantiate()
  syn <- attempt_login(syn, ...)
  return(syn)
}
