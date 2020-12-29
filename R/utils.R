## Check if we are running on travis
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

## Attempt to log in using encrypted variables written to .synapseConfig file
## within Sage org, or with regular synLogin() if not on CI. If on CI but not
## within Sage, do nothing.
attempt_login <- function(syn, ...) {
  if (on_ci() & !is.null(syn)) {
    try(syn$login(), silent = TRUE)
  } else if (reticulate::py_module_available("synapseclient") & !is.null(syn)) {
    syn$login(...)
    return(syn)
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

## Remove rows containing all NAs from a data frame
remove_empty_rows <- function(d) {
  dplyr::filter(
    dplyr::rowwise(d),
    !all(is.na(dplyr::c_across(dplyr::everything())))
  ) %>%
    dplyr::ungroup()
}
