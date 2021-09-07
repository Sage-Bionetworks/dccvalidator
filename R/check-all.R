#' Run all validation checks
#'
#' Runs all validation checks. Requires an environment configuration
#' (config) to be set. The config is expected to have templates for
#' each metadataType, where individual and biospecimen depend on species and
#' assay depends on the assay type. Additionally, there should be
#' complete_columns for each metadataType.
#'
#' @param data A tibble or dataframe with the columns:
#'   name, metadataType, species, assay, file_data, template (optional).
#'   The file_data column should be a list column containing
#'   a dataframe with the file data or `NULL` if the data
#'   does not exist. `data` is expected to have four rows,
#'   one for each metadataType: individual, biospecimen,
#'   assay, manifest. If file_data is `NULL` for a given
#'   metadataType, the metadataType should still be
#'   present. The template column should have the synID or Synapse JSON schema
#'   id for the metadata temaplate. If this is `NULL` or the column is missing,
#'   then the check will not verify that all expected columns are present in
#'   the metadata (i.e. `check_cols` is skipped).
#' @param study A string containing the name of the study (default `NA`). If not
#' given, will not check the metadata for individuals and specimens currently
#' in the `samples_table`.
#' @param samples_table Synapse synID for a table containing the columns:
#' specimenID, individualID, assay, study (default `NA`). If `samples_table` or
#' `study` not provided, will not check the metadata for individuals and
#' specimens currently in this table.
#' @inheritParams check_annotation_keys
#' @return List of conditions
#' @export
#' @examples
#' \dontrun{
#' syn <- synapse$Synapse()
#' syn$login()
#'
#' annots <- get_synapse_annotations(syn = syn)
#'
#' data <- tibble::tibble(
#'   metadataType = c(
#'     "manifest",
#'     "individual",
#'     "biospecimen",
#'     "assay"
#'   ),
#'   name = c("a", NA, NA, "c"),
#'   species = "human",
#'   assay = "rnaSeq",
#'   file_data = c(
#'     list(data.frame(a = c(TRUE, FALSE), b = c(1, 3))),
#'     list(NULL),
#'     list(NULL),
#'     list(data.frame(a = c(TRUE, FALSE), b = c(1, 3)))
#'   )
#' )
#' res <- check_all(data, annots, syn)
#' }
check_all <- function(data, annotations, syn, study = NA, samples_table = NA) {

  # Get indices by type
  indices <- get_metadataType_indices(
    data,
    c("individual", "biospecimen", "assay", "manifest")
  )

  # Must have 1 and only 1 index per metadata type
  num_indices <- purrr::map_lgl(indices, ~ length(.) != 1)
  if (length(indices) < 4 | any(num_indices)) {
    stop("There must be exactly row 1 of each metadata type to check: biospecimen, individual, assay, manifest") # nolint
  }

  # Missing columns ------------------------------------------------------------
  # Only run if the template column exists
  missing_cols_indiv <- missing_cols_biosp <- missing_cols_assay <- missing_cols_manifest <- NULL # nolint
  if ("template" %in% colnames(data)) {
    missing_cols_indiv <- check_cols_individual(
      data$file_data[indices$individual][[1]],
      data$template[indices$individual][[1]],
      syn = syn
    )
    missing_cols_biosp <- check_cols_biospecimen(
      data$file_data[indices$biospecimen][[1]],
      data$template[indices$biospecimen][[1]],
      syn = syn
    )
    missing_cols_assay <- check_cols_assay(
      data$file_data[indices$assay][[1]],
      data$template[indices$assay][[1]],
      syn = syn
    )
    missing_cols_manifest <- check_cols_manifest(
      data$file_data[indices$manifest][[1]],
      data$template[indices$manifest][[1]],
      syn = syn
    )
  }

  # Individual and specimen IDs match ----------------------------------------
  individual_ids_indiv_biosp <- check_indiv_ids_match(
    data$file_data[indices$individual][[1]],
    data$file_data[indices$biospecimen][[1]],
    "individual",
    "biospecimen",
    bidirectional = FALSE
  )
  individual_ids_indiv_manifest <- check_indiv_ids_match(
    data$file_data[indices$individual][[1]],
    data$file_data[indices$manifest][[1]],
    "individual",
    "manifest",
    bidirectional = FALSE
  )
  specimen_ids_biosp_assay <- check_specimen_ids_match(
    data$file_data[indices$biospecimen][[1]],
    data$file_data[indices$assay][[1]],
    "biospecimen",
    "assay",
    bidirectional = FALSE
  )
  specimen_ids_biosp_manifest <- check_specimen_ids_match(
    data$file_data[indices$biospecimen][[1]],
    data$file_data[indices$manifest][[1]],
    "biospecimen",
    "manifest",
    bidirectional = FALSE
  )

  # Annotation keys in manifest are valid ------------------------------------
  annotation_keys_manifest <- check_annotation_keys(
    data$file_data[indices$manifest][[1]],
    annotations,
    allowlist_keys = c("path", "parent", "name", "used", "executed"),
    success_msg = "All keys (column names) in the manifest are valid",
    fail_msg = "Some keys (column names) in the manifest are invalid",
    annots_link = get_golem_config("annotations_link")
  )

  # Annotation values in manifest and metadata are valid ---------------------
  annotation_values_manifest <- check_annotation_values(
    data$file_data[indices$manifest][[1]],
    annotations,
    success_msg = "All values in the manifest are valid",
    fail_msg = "Some values in the manifest are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_indiv <- check_annotation_values(
    data$file_data[indices$individual][[1]],
    annotations,
    allowlist_keys = c("individualID"),
    success_msg = "All values in the individual metadata are valid",
    fail_msg = "Some values in the individual metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_biosp <- check_annotation_values(
    data$file_data[indices$biospecimen][[1]],
    annotations,
    allowlist_keys = c("specimenID", "individualID"),
    success_msg = "All values in the biospecimen metadata are valid",
    fail_msg = "Some values in the biospecimen metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_assay <- check_annotation_values(
    data$file_data[indices$assay][[1]],
    annotations,
    allowlist_keys = c("specimenID"),
    success_msg = "All values in the assay metadata are valid",
    fail_msg = "Some values in the assay metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )

  # Individual and specimen IDs are not duplicated ---------------------------
  duplicate_indiv_ids <- check_indiv_ids_dup(
    data$file_data[indices$individual][[1]],
    success_msg = "Individual IDs in the individual metadata file are unique",
    fail_msg = "Duplicate individual IDs found in the individual metadata file"
  )
  duplicate_specimen_ids <- check_specimen_ids_dup(
    data$file_data[indices$biospecimen][[1]],
    success_msg = "Specimen IDs in the biospecimen metadata file are unique",
    fail_msg = "Duplicate specimen IDs found in the biospecimen metadata file"
  )

  # Empty columns produce warnings -------------------------------------------
  empty_cols_manifest <- check_cols_empty(
    data$file_data[indices$manifest][[1]],
    required_cols = get_golem_config("complete_columns")$manifest,
    success_msg = "No columns are empty in the manifest",
    fail_msg = "Some columns are completely empty in the manifest"
  )
  empty_cols_indiv <- check_cols_empty(
    data$file_data[indices$individual][[1]],
    required_cols = get_golem_config("complete_columns")$individual,
    success_msg = "No columns are empty in the individual metadata",
    fail_msg = "Some columns are completely empty in the individual metadata"
  )
  empty_cols_biosp <- check_cols_empty(
    data$file_data[indices$biospecimen][[1]],
    required_cols = get_golem_config("complete_columns")$biospecimen,
    success_msg = "No columns are empty in the biospecimen metadata",
    fail_msg = "Some columns are completely empty in the biospecimen metadata"
  )
  empty_cols_assay <- check_cols_empty(
    data$file_data[indices$assay][[1]],
    required_cols = get_golem_config("complete_columns")$assay,
    success_msg = "No columns are empty in the assay metadata",
    fail_msg = "Some columns are completely empty in the assay metadata"
  )

  # Incomplete required columns produce failures -----------------------------
  complete_cols_manifest <- check_cols_complete(
    data$file_data[indices$manifest][[1]],
    required_cols = get_golem_config("complete_columns")$manifest,
    success_msg = "There is no missing data in columns that are required to be complete in the manifest", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the manifest" # nolint
  )
  complete_cols_indiv <- check_cols_complete(
    data$file_data[indices$individual][[1]],
    required_cols = get_golem_config("complete_columns")$individual,
    success_msg = "There is no missing data in columns that are required to be complete in the individual metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the individual metadata" # nolint
  )
  complete_cols_biosp <- check_cols_complete(
    data$file_data[indices$biospecimen][[1]],
    required_cols = get_golem_config("complete_columns")$biospecimen,
    success_msg = "There is no missing data in columns that are required to be complete in the biospecimen metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the biospecimen metadata" # nolint
  )
  complete_cols_assay <- check_cols_complete(
    data$file_data[indices$assay][[1]],
    required_cols = get_golem_config("complete_columns")$assay,
    success_msg = "There is no missing data in columns that are required to be complete in the assay metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the assay metadata" # nolint
  )

  # Metadata files appear in manifest ----------------------------------------
  meta_files_in_manifest <- check_files_manifest(
    data$file_data[indices$manifest][[1]],
    c(
      data$name[indices$individual],
      data$name[indices$biospecimen],
      data$name[indices$assay]
    ),
    success_msg = "Manifest file contains all metadata files",
    fail_msg = "Manifest file does not contain all metadata files"
  )

  # Parent column in manifest is valid synID -----------------------------------
  valid_parent_syn <- check_parent_syn(data$file_data[indices$manifest][[1]])

  # Ages over 90 are censored in human individual metadata ---------------------
  if (any(data$species == "human", na.rm = TRUE)) {
    ages_over_90_indiv <- check_ages_over_90(
      data$file_data[indices$individual][[1]],
      success_msg = "No ages over 90 detected in the individual metadata",
      fail_msg = "Ages over 90 detected in the individual metadata"
    )
    ages_over_90_biosp <- check_ages_over_90(
      data$file_data[indices$biospecimen][[1]],
      col = "samplingAge",
      success_msg = "No ages over 90 detected in the biospecimen metadata",
      fail_msg = "Ages over 90 detected in the biospecimen metadata"
    )
  } else {
    ages_over_90_biosp <- ages_over_90_indiv <- NULL
  }

  # No file paths are duplicated in the manifest -------------------------------
  duplicate_file_paths <- check_duplicate_paths(
    data$file_data[indices$manifest][[1]]
  )

  # Additions to existing studies have complete IDs ----------------------------
  ## Only do if samples_table and study provided
  complete_ids_indiv <- complete_ids_biosp <- complete_ids_assay <- NULL
  if (!is.na(samples_table) & !is.na(study)) {
    samples <- syn$tableQuery(
      glue::glue("SELECT * FROM {samples_table} WHERE study = '{study}'"),
      includeRowIdAndRowVersion = FALSE
    )
    samples <- readr::read_csv(samples$filepath)
    ## Check if the study is in the table before continuing
    if (study %in% samples$study) {
      assay <- data[indices$assay, "assay", drop = TRUE]
      complete_ids_indiv <- check_complete_ids(
        data$file_data[indices$individual][[1]],
        samples_table = samples,
        study = study,
        id_type = "individualID",
        success_msg = "All pre-existing individual IDs are present in the individual file", # nolint
        fail_msg = "Some individual IDs that were previously part of this study are missing from the individual file" # nolint
      )
      complete_ids_biosp <- check_complete_ids(
        data$file_data[indices$biospecimen][[1]],
        samples_table = samples,
        study = study,
        id_type = "specimenID",
        success_msg = "All pre-existing specimen IDs are present in the biospecimen file", # nolint
        fail_msg = "Some specimen IDs that were previously part of this study are missing from the biospecimen file" # nolint
      )
      complete_ids_assay <- check_complete_ids(
        data$file_data[indices$assay][[1]],
        samples_table = samples,
        study = study,
        id_type = "specimenID",
        assay = assay,
        success_msg = "All pre-existing specimen IDs for this assay are present in the assay file", # nolint
        fail_msg = "Some specimen IDs that were previously part of this study and assay are missing from the assay file" # nolint
      )
    }
  }

  ## List results
  res <- list(
    missing_cols_indiv = missing_cols_indiv,
    missing_cols_biosp = missing_cols_biosp,
    missing_cols_assay = missing_cols_assay,
    missing_cols_manifest = missing_cols_manifest,
    individual_ids_indiv_biosp = individual_ids_indiv_biosp,
    individual_ids_indiv_manifest = individual_ids_indiv_manifest,
    specimen_ids_biosp_assay = specimen_ids_biosp_assay,
    specimen_ids_biosp_manifest = specimen_ids_biosp_manifest,
    annotation_keys_manifest = annotation_keys_manifest,
    annotation_values_manifest = annotation_values_manifest,
    annotation_values_indiv = annotation_values_indiv,
    annotation_values_biosp = annotation_values_biosp,
    annotation_values_assay = annotation_values_assay,
    duplicate_indiv_ids = duplicate_indiv_ids,
    duplicate_specimen_ids = duplicate_specimen_ids,
    empty_cols_manifest = empty_cols_manifest,
    empty_cols_indiv = empty_cols_indiv,
    empty_cols_biosp = empty_cols_biosp,
    empty_cols_assay = empty_cols_assay,
    complete_cols_manifest = complete_cols_manifest,
    complete_cols_indiv = complete_cols_indiv,
    complete_cols_biosp = complete_cols_biosp,
    complete_cols_assay = complete_cols_assay,
    meta_files_in_manifest = meta_files_in_manifest,
    valid_parent_syn = valid_parent_syn,
    ages_over_90_indiv = ages_over_90_indiv,
    ages_over_90_biosp = ages_over_90_biosp,
    duplicate_file_paths = duplicate_file_paths,
    complete_ids_indiv = complete_ids_indiv,
    complete_ids_biosp = complete_ids_biosp,
    complete_ids_assay = complete_ids_assay
  )
  res
}

## Check all for invalid characters
check_all_invalid_char <- function(manifest, indiv, biosp, assay) {
  
  # Invalid characters ---------------------------------------------------------
  invalid_characters_manifest <- check_invalid_characters(
    manifest,
    success_msg = "There are no invalid characters in the manifest",
    fail_msg = "There are invalid characters in the manifest columns"
  )
  invalid_characters_individual <- check_invalid_characters(
    indiv,
    success_msg = "There are no invalid characters in the individual metadata",
    fail_msg = "There are invalid characters in the individual metadata columns"
  )
  invalid_characters_biospecimen <- check_invalid_characters(
    biosp,
    success_msg = "There are no invalid characters in the biospecimen metadata",
    fail_msg = "There are invalid characters in the biospecimen metadata columns" #nolint
  )
  invalid_characters_assay <- check_invalid_characters(
    assay,
    success_msg = "There are no invalid characters in the assay metadata",
    fail_msg = "There are invalid characters in the assay metadata columns"
  )
  list(
    invalid_characters_manifest = invalid_characters_manifest,
    invalid_characters_individual = invalid_characters_individual,
    invalid_characters_biospecimen = invalid_characters_biospecimen,
    invalid_characters_assay = invalid_characters_assay
  )
}

## Summarize all invalid character checks
summarize_invalid_char_check <- function(check_list) {
  ## Only checks that are check_fail
  failed <- purrr::map_lgl(check_list, ~ inherits(., "check_fail"))
  failed_text <- purrr::map_chr(check_list[failed], ~ summarize_failed_check(.))
  glue::glue_collapse(failed_text, sep = "\n")
}

summarize_check <- function(check_result) {
  details <- glue::glue_collapse(check_result$data, sep = ", ")
  glue::glue("{check_result$message} {details}")
}
