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
  } else {         # nocov start
    return(FALSE)
  }                # nocov end
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
