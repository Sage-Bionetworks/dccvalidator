## Look up env vars and log in to Synapse
syn_travis_login <- function() {
  ## Credentials are encrypted on travis
  user <- Sys.getenv("SYNAPSE_USER")
  pass <- Sys.getenv("SYNAPSE_PASSWORD")
  synapser::synLogin(email = user, password = pass)
}

## Check if we are running on travis
on_travis <- function() {
  if (!identical(Sys.getenv("TRAVIS"), "true")) {
    return(TRUE)
  }
}
