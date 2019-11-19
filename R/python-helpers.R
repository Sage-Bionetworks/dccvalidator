## Convert dict of annotations (returned by syn$getAnnotations()) into a named
## list

#' @importFrom reticulate py_to_r
py_to_r.synapseclient.annotations.Annotations <- function(x) { # nolint
  stats::setNames(x$values(), x$keys())
}
