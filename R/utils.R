## Check if we are running on travis
on_travis <- function() {
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    return(TRUE)
    # nocov start
  } else {
    return(FALSE)
  }
  # nocov end
}

## Look up env vars and log in to Synapse
syn_travis_login <- function(syn) {
  ## Credentials are encrypted on travis
  user <- Sys.getenv("SYNAPSE_USER")
  pass <- Sys.getenv("SYNAPSE_PASSWORD")
  syn$login(email = user, password = pass)
}

attempt_instantiate <- function() {
  if (reticulate::py_module_available("synapseclient")) {
    return(synapse$Synapse())
  } else {
    return(NULL)
  }
}

## Attempt to log in using encrypted travis variables if on travis within Sage
## org, or with regular synLogin() if not on travis. If on travis but not within
## Sage, do nothing.
attempt_login <- function(syn, ...) {
  if (on_travis() & !is.null(syn)) {
    try(syn_travis_login(syn), silent = TRUE)
  } else if (reticulate::py_module_available("synapseclient") & !is.null(syn)) {
    syn$login(...)
  } else {
    return(NULL)
  }
}

## Check if we're logged in
logged_in <- function(syn) {
  if (is.null(syn) || is.null(syn$username)) {
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
