#' Checks that the name of the study is valid.
#'
#' @param study_name Name of the study.
#' @return TRUE if study_name is valid; FALSE otherwise.
is_study_name_valid <- function(study_name) {
  study_valid <- TRUE
  if (study_name == "") {
    study_valid <- FALSE
  }
  # Check if study name has inappropriate characters
  temp_string <- stringr::str_replace_all(
    study_name,
    " |\\.|_|-|\\+|\\(|\\)",
    ""
  )
  if (grepl("[[:punct:]]", temp_string)) {
    study_valid <- FALSE
  }
  study_valid
}
