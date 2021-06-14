#' Modal with next step
#'
#' If none of the checks inherit `check_fail`, then pop up a modal to tell
#' users what to do next.
#'
#' @param results List of conditions
#' @param email Contact email as a string
#' @noRd
next_step_modal <- function(results, email) {
  is_failure <- purrr::map_lgl(results, function(x) {
    inherits(x, "check_fail")
  })
  if (!any(is_failure)) {
    showModal(
      modalDialog(
        title = "Great work!",
        HTML(
          glue::glue(
            "Your validated file(s) had no failures. Please contact <a target=\"_blank\" href=\"{email}\">{email}</a> to proceed with the next step if you have validated all finalized metadata and manifest files at once. For multiple assays, please validate each assay with your other metadata files (individual and/or biospecimen) and manifest." # nolint
          )
        ),
        easyClose = TRUE
      )
    )
  }
}
