#' @title Get metadata template keys
#'
#' @description Get the keys from a metadata template stored as an excel or csv
#' or registered as a JSON schema in Synapse.
#'
#' @param synID Synapse ID of an excel or csv file containing a metadata
#'   template
#' @param id The id of JSON metadata schema registered in Synapse or Synapse ID
#' of an excel or csv file containing a metadata template. Or a filepath or URL
#' to a json schema.
#' @inheritParams get_synapse_annotations
#' @param ... Additional arguments passed to syn$get()
#' @return Character vector of template column names
#' @export
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' get_template(synID = "syn12973252", syn = syn)
#' get_template(id = "syn12973252", syn = syn)
#' }
get_template <- function(id = NA, syn, synID = NA, ...) {
  ## Use synID if given
  if (!is.na(synID)) {
    return(get_template_keys_synID(syn = syn, synID = synID, ...))
  }
  if (!is.na(id)) {
    ## Check if id was used to give a synID
    if (grepl("^syn", id)) {
      return(get_template_keys_synID(syn = syn, synID = id, ...))
    }
    ## Check if a schema file or URL was passed
    if (identical(tolower(tools::file_ext(id)), "json") | grepl("/", id)) {
      return(get_template_keys_schema(syn = syn, file = id))
    }
    ## If not a synID, assume a schema id
    return(get_template_keys_schema(syn = syn, id = id))
  }
  ## Either synID is weird or both are NA; just return
  return(NA)
}

#' @title Get excel or csv template
#'
#' @description Get the metadata keys from an excel or csv template stored in
#' Synapse.
#'
#' @param synID Synapse ID of an excel or csv file containing a metadata
#'   template
#' @inheritParams get_synapse_annotations
#' @param ... Additional arguments passed to syn$get()
#' @return Character vector of template column names
#' @export
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#' get_template("syn12973252", syn = syn)
#' }
get_template_keys_synID <- function(syn, synID, ...) {
  template <- try(syn$get(synID, ...), silent = TRUE)
  if (inherits(template, "try-error")) {
    stop(
      "Couldn't download metadata template. Make sure you are logged in to Synapse and that `synID` is a valid synapse ID.", # nolint
      .call = FALSE
    )
  }

  filepath <- template$path
  ext <- tools::file_ext(filepath)

  if (!ext %in% c("xlsx", "csv")) {
    stop(
      "Error loading template: file format must be .csv or .xlsx",
      call. = FALSE
    )
  }

  if (ext == "xlsx") {
    template <- readxl::read_excel(template$path, sheet = 1)
  } else if (ext == "csv") {
    template <- utils::read.csv(template$path, stringsAsFactors = FALSE)
  }
  return(names(template))
}

#' @title Get JSON schema template
#'
#' @description Get JSON schema registered in Synapse.
#'
#' @export
#' @inheritParams get_template
#' @param id Registered synapse ID of schema.
#' @param file Filepath or URL to json schema.
get_template_keys_schema <- function(syn, id = NA, file = NA) {
  
  if (sum(!is.na(id), !is.na(file)) != 1){
    stop("Specify only one of id, file")
  }
  
  if (!is.na(id)){
    tryCatch(
      {
        schema <- get_synapse_schema(syn = syn, id = id)
      },
      error = function(e) {
        stop(glue::glue("Failed to get the validation schema for {id}."))
      }
    )
  }
  if (!is.na(file)) {
    schema <- get_file_schema(file = file)
  }
  return(names(schema$properties))
}

#' @title Get Synapse validation schema
#'
#' @description Get Synapse validation schema.
#'
#' @export
#' @inheritParams get_template
get_synapse_schema <- function(syn, id) {
  ## Start the job to compile the validation schema
  tryCatch(
    {
      token <- rest_post(
        syn = syn,
        uri = "/schema/type/validation/async/start",
        body = glue::glue("{{$id: \"{id}\"}}")
      )
      ## Should wait until finished
      schema <- get_synapse_schema_compiled(syn = syn, token = token)
      return(schema$validationSchema)
    },
    error = function(e) {
      stop("The JSON schema could not be returned.")
    }
  )
}

#' @title Get compiled validation schema
#'
#' @description Get the compiled validation schema. This is a helper used by
#' [get_synapse_schema] to check the status and only send back the schema when
#' finished compiling.
#'
#' @noRd
#' @inheritParams get_synapse_schema
#' @param token Token response from Synapse for kicking off the compiling of a
#' validation schema.
get_synapse_schema_compiled <- function(syn, token) {
  processing <- TRUE
  ## Check results of registering schema, retrying if the async job hasn't
  ## completed yet
  while (processing) {
    result <- rest_get(
      syn = syn,
      uri = glue::glue("/schema/type/validation/async/get/{token}")
    )
    ## synapser doesn't return the status codes unfortunately, so we check the
    ## response object to determine what to do. If it contains "jobState",
    ## that's part of the AsynchronousJobStatus, i.e. the async job hasn't
    ## completed yet.
    if (!"jobState" %in% names(result)) {
      processing <- FALSE
    }
  }
  result
}

#' @title restGET wrapper
#'
#' @description Wrapper for the Synapse restGET API call for easier mocking.
#'
#' @noRd
#' @inheritParams get_template
#' @param uri The uri for the API call
rest_get <- function(syn, uri) {
  syn$restGET(uri = uri)
}

#' @title restPOST wrapper
#'
#' @description Wrapper for the Synapse restPOST API call.
#'
#' @noRd
#' @inheritParams rest_get
rest_post <- function(syn, uri, body = NULL) {
  if (is.null(body)) {
    # synapseclient requires a body for all restPOST calls, even if API doesn't
    syn$restPOST(uri = uri, body = "")
  } else {
    syn$restPOST(uri = uri, body = body)
  }
}

#' @title Get JSON schema attributes from a file
#' 
#' @description Read a JSON schema from a file. Similar
#' to [get_synapse_schema].
#' 
#' @param file Filepath of json schema.
get_file_schema <- function(file) {
  
  schema <- jsonlite::fromJSON(txt = file)
  return(schema)
  
}
