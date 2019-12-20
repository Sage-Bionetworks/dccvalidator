## Convert dict of annotations (returned by syn$getAnnotations()) into a named
## list
dict_to_list <- function(x) {
  ## Not sure why x$values() doesn't work here, but it doesn't so we're
  ## iterating to get the values instead :-/
  values <- purrr::map(names(x), function(y) x$get(y))
  stats::setNames(values, names(x))
}
