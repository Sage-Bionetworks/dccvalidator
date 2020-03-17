#' Check synID of parent in manifest
#'
#' Check that the synapse ID in the manifest matches a pattern of `syn` followed
#' by numbers.
#'
#' @param manifest The manifest as a data frame or tibbl
#' @inheritParams check_values
#' @return A condition object indicating whether the files are present in the
#'   `path` column of the manifest
#' @export
#' @examples
#' manifest <- data.frame(parent = c("syn", "syn123"), stringsAsFactors = FALSE)
#' check_parent_syn(manifest)
check_parent_syn <- function(manifest,
                             # nolint start
                             success_msg = "All Synapse IDs in the manifest are valid",
                             fail_msg = "Some Synapse IDs in the manifest are invalid") {
  # nolint end
  if (is.null(manifest)) {
    return(NULL)
  }
  if (!"parent" %in% names(manifest)) {
    return(
      check_fail(
        msg = "Manifest is missing `parent` column",
        behavior = "Manifest should contain a column called `parent`"
      )
    )
  }
  behavior <- "Synapse IDs should follow the format 'syn' followed by digits"
  ## Regex pattern to match "syn" + 1 or more digits
  matches <- grepl("^syn[[:digit:]]+", manifest$parent)
  if (!all(matches)) {
    res <- check_fail(
      msg = fail_msg,
      behavior = behavior,
      data = manifest$parent[!matches]
    )
  } else {
    res <- check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
  res
}
