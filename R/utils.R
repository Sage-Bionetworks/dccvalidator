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
  attempt_login(syn, ...)
  return(syn)
}
