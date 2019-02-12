## Look up env vars and log in to Synapse
syn_travis_login <- function() {
  ## Credentials are encrypted on travis
  user <- Sys.getenv("SYNAPSE_USER")
  pass <- Sys.getenv("SYNAPSE_PASSWORD")
  synapser::synLogin(email = user, password = pass)
}

## Check if we are running on travis
on_travis <- function() {
  if (identical(Sys.getenv("TRAVIS"), "true")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## Get the value of an annotation on object x
get_annotation <- function(x, key) {
  if (missing(key)) {
    stop("Please provide an annotation key to look up", call. = FALSE)
  }
  synapser::synGetAnnotations(x)[[key]]
}
