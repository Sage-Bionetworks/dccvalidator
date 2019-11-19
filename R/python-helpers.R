## Convert dict of annotations (returned by syn$getAnnotations()) into a named
## list
dict_to_list <- function(x) {
  stats::setNames(x$values(), x$keys())
}
