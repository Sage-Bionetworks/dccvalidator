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
    if (!on_fork()) {
      syn_travis_login()
    }
  } else {
    synapser::synLogin(...)
  }
}

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

## Check if we're on a fork
on_fork <- function() {
  slug <- Sys.getenv("TRAVIS_REPO_SLUG")
  owner <- gsub("(^[^/]+)(.+)", "\\1", slug)
  if (owner != "Sage-Bionetworks") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## Get the value of an annotation on object(s) x
get_annotation <- function(ids, key) {
  if (missing(key)) {
    stop("Please provide an annotation key to look up", call. = FALSE)
  }
  ids <- purrr::set_names(ids)
  annots <- purrr::map(ids, function(x) synapser::synGetAnnotations(x)[[key]])
  unlist(annots)
}

## Save uploaded files to Synapse
save_to_synapse <- function(input_file, parent, name = NULL) {
  file_to_upload <- synapser::File(
    input_file$datapath,
    parent = parent,
    name = name
  )
  synapser::synStore(file_to_upload)
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

#' Skip running a test on Travis from a forked repo
#'
#' Forked repos don't have access to the encrypted environment variables Travis
#' uses to run some tests. This function can be used to skip tests on builds
#' from forks.
skip_on_fork <- function() {
  if (!on_travis()) {
    return(invisible(TRUE))
  }
  if (!on_fork()) {
    return(invisible(TRUE))
  }
  testthat::skip("On a fork without access to encryped travis variables")
}
