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
syn_travis_login <- function() {
  ## Credentials are encrypted on travis
  user <- Sys.getenv("SYNAPSE_USER")
  pass <- Sys.getenv("SYNAPSE_PASSWORD")
  synapser::synLogin(email = user, password = pass)
}

## Attempt to log in using encrypted travis variables if on travis within Sage
## org, or with regular synLogin() if not on travis. If on travis but not within
## Sage, do nothing.
attempt_login <- function(...) {
  if (on_travis()) {
    try(syn_travis_login(), silent = TRUE)
  } else {
    synapser::synLogin(...)
  }
}

## Check if we're logged in
logged_in <- function() {
  if (is.null(PythonEmbedInR::pyGet("syn.username"))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

## Save uploaded files to Synapse
save_to_synapse <- function(input_file,
                            parent,
                            annotations = NULL) {
  if (!is_name_valid(input_file$name)) {
    stop("Please check that file names only contain: letters, numbers, spaces, underscores, hyphens, periods, plus signs, and parentheses.") # nolint
  }
  file_to_upload <- synapser::File(
    input_file$datapath,
    parent = parent,
    name = input_file$name,
    annotations = annotations
  )
  synapser::synStore(file_to_upload)
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
