#' Check that files are present in manifest
#'
#' Given a manifest and vector of file names, checks that the file names appear
#' in the manifest. This is useful to ensure that metadata files (not just data
#' files) are included in the manifest for upload.
#'
#' @param manifest The manifest as a data frame or tibble
#' @param filenames File names to look for in the `path` column of the manifest
#' @param strict If `FALSE`, return a `"check_warn"` object; if `TRUE`, return a
#'   `"check_fail"` object
#' @inheritParams check_values
#' @return A condition object indicating whether the files are present in the
#'   `path` column of the manifest
#' @export
#' @examples
#' manifest <- data.frame(
#'   path = c("individual_metadata.csv", "biospecimen_metadata.csv"),
#'   parent = c("syn123", "syn123")
#' )
#' check_files_manifest(
#'   manifest,
#'   c(
#'     "individual_metadata.csv",
#'     "biospecimen_metadata.csv",
#'     "assay_metadata.csv"
#'   )
#' )
check_files_manifest <- function(manifest, filenames, strict = FALSE,
                                 # nolint start
                                 success_msg = "All required files are present in manifest",
                                 fail_msg = "Some files may be missing from manifest") {
  # nolint end
  if (is.null(manifest) || is.null(filenames)) {
    return(NULL)
  }
  if (!"path" %in% names(manifest)) {
    res <- check_fail(
      msg = "Manifest is missing `path` column",
      behavior = "Manifest should contain a column called `path`"
    )
    return(res)
  }
  ## Convert factor column if needed
  if (is.factor(manifest$path)) {
    manifest$path <- as.character(manifest$path)
  }
  names_collapsed <- glue::glue_collapse(filenames, sep = ", ")
  behavior <- glue::glue(
    "The following files should be present in the manifest: {names_collapsed}. However, if this is an update to an existing study and the files have not changed since they were last released, this warning can be ignored." # nolint
  )
  if (all(is.na(manifest$path))) {
    missing <- filenames
  } else {
    missing <- setdiff(filenames, basename(manifest$path))
  }
  if (length(missing) > 0) {
    check_condition(
      msg = fail_msg,
      behavior = behavior,
      data = missing,
      type = ifelse(strict, "check_fail", "check_warn")
    )
  } else {
    check_pass(
      msg = success_msg,
      behavior = behavior
    )
  }
}
