#' Check data against existing IDs
#'
#' If data is an addition to an existing study, check the specimen and
#' individual IDs against the master table of existing IDs. Contributors should
#' upload the complete metadata files, so all IDs in the master table should be
#' present in the uploaded data.
#'
#' @inheritParams check_values
#' @param data Data frame of data to be checked
#' @param samples_table Data frame containing master table of all individual and
#'   specimen IDs per study
#' @param study Name of the study
#' @param id_type `"individualID"` or `"specimenID"`
#' @param assay String naming the assay. If provided, will filter `master_table`
#'   to the specific study and assay. This is useful when validating assay
#'   metadata in studies wheere multiple assays with different specimens were
#'   conducted. If `NULL` (the default), will look at all IDs for the study,
#'   regardless of assay.
#' @return A condition object indicating whether the required columns were
#'   present (`"check_pass"`) or absent (`"check_fail"`)
#' @export
#' @examples
#' samples_table <- tibble::tribble(
#'   ~individualID, ~specimenID,   ~study,   ~assay,
#'              "A",        "a1", "study1", "rnaSeq",
#'              "A",        "a2", "study1", "rnaSeq",
#'              "B",        "b1", "study1", "rnaSeq",
#'              "B",        "b2", "study1", "rnaSeq",
#'              "B",        "b1", "study1", "rnaSeq",
#'              "B",        "b1", "study1", "rnaSeq",
#'              "C",        "c1", "study1",  "LC-MS",
#'              "D",        "d1", "study2", "rnaSeq",
#'              "D",        "d2", "study2", "rnaSeq"
#' )
#' dat <- tibble::tribble(
#'   ~individualID, ~specimenID,
#'              "A",        "a1",
#'              "A",        "a2"
#' )
#' check_complete_ids(
#'   data = dat,
#'   samples_table = samples_table,
#'   id_type = "individualID",
#'   study = "study1"
#' )
check_complete_ids <- function(data, samples_table, study,
                               id_type = c("individualID", "specimenID"),
                               assay = NULL,
                               success_msg = "Data includes a complete set of IDs", # nolint
                               fail_msg = "Some IDs are missing from the data") { # nolint

  if (is.null(data)) {
    return(NULL)
  }
  behavior <- "If the data being validated is an addition to an existing study, all IDs that were previously shared for this study should be included in the metadata" # nolint
  stopifnot(study %in% samples_table$study)
  if (!id_type %in% names(data)) {
    return(
      check_fail(
        msg = glue::glue("Can't check for complete IDs because {id_type} column is missing from data"), # nolint
        behavior = behavior,
        data = NULL
      )
    )
  }
  samples_table <- samples_table[samples_table$study == study, ]
  if (!is.null(assay)) {
    samples_table <- samples_table[samples_table$assay == assay, ]
  }
  missing <- setdiff(
    samples_table[, id_type, drop = TRUE],
    data[, id_type, drop = TRUE]
  )
  if (length(missing) > 0) {
    check_fail(msg = fail_msg, behavior = behavior, data = missing)
  } else {
    check_pass(msg = success_msg, behavior = behavior)
  }
}
