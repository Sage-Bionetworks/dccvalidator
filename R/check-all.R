#' Run all validation checks
#'
#' Runs all validation checks. Requires an environment configuration
#' (config) to be set. The config is expected to have templates for
#' each metadataType, where individual and biospecimen depend on species and
#' assay depends on the assay type. Additionally, there should be
#' complete_columns for each metadataType.
#'
#' @param data A tibble or dataframe with the columns:
#'   name, metadataType, species, assay, file_data.
#'   The file_data column should be a list column containing
#'   a dataframe with the file data or `NULL` if the data
#'   does not exist. `data` is expected to have four rows,
#'   one for each metadataType: individual, biospecimen,
#'   assay, manifest. If file_data is `NULL` for a given
#'   metadataType, the metadataType should still be
#'   present.
#' @param study A string containing the name of the study.
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
check_all <- function(data, annotations, study, syn) {

  # Get indices by type
  indiv_index <- which(data$metadataType == "individual")
  biosp_index <- which(data$metadataType == "biospecimen")
  assay_index <- which(data$metadataType == "assay")
  manifest_index <- which(data$metadataType == "manifest")

  # Must have 1 and only 1 index per metadata type
  purrr::walk(
    list(indiv_index, biosp_index, assay_index, manifest_index),
    function(x) {
      if (length(x) != 1) {
        stop("There must be exactly row 1 of each metadata type to check: biospecimen, individual, assay, manifest") # nolint
      }
    }
  )

  # Missing columns ----------------------------------------------------------
  missing_cols_indiv <- check_cols_individual(
    data$file_data[indiv_index][[1]],
    get_golem_config("templates")$individual_templates[[data$species[indiv_index]]],
    syn = syn
  )
  missing_cols_biosp <- check_cols_biospecimen(
    data$file_data[biosp_index][[1]],
    get_golem_config("templates")$biospecimen_templates[[data$species[biosp_index]]],
    syn = syn
  )
  missing_cols_assay <- check_cols_assay(
    data$file_data[assay_index][[1]],
    get_golem_config("templates")$assay_templates[[data$assay[assay_index]]],
    syn = syn
  )
  missing_cols_manifest <- check_cols_manifest(
    data$file_data[manifest_index][[1]],
    get_golem_config("templates")$manifest_template,
    syn = syn
  )

  # Individual and specimen IDs match ----------------------------------------
  individual_ids_indiv_biosp <- check_indiv_ids_match(
    data$file_data[indiv_index][[1]],
    data$file_data[biosp_index][[1]],
    "individual",
    "biospecimen",
    bidirectional = FALSE
  )
  individual_ids_indiv_manifest <- check_indiv_ids_match(
    data$file_data[indiv_index][[1]],
    data$file_data[manifest_index][[1]],
    "individual",
    "manifest",
    bidirectional = FALSE
  )
  specimen_ids_biosp_assay <- check_specimen_ids_match(
    data$file_data[biosp_index][[1]],
    data$file_data[assay_index][[1]],
    "biospecimen",
    "assay",
    bidirectional = FALSE
  )
  specimen_ids_biosp_manifest <- check_specimen_ids_match(
    data$file_data[biosp_index][[1]],
    data$file_data[manifest_index][[1]],
    "biospecimen",
    "manifest",
    bidirectional = FALSE
  )

  # Annotation keys in manifest are valid ------------------------------------
  annotation_keys_manifest <- check_annotation_keys(
    data$file_data[manifest_index][[1]],
    annotations,
    allowlist_keys = c("path", "parent", "name", "used", "executed"),
    success_msg = "All keys (column names) in the manifest are valid",
    fail_msg = "Some keys (column names) in the manifest are invalid",
    annots_link = get_golem_config("annotations_link")
  )

  # Annotation values in manifest and metadata are valid ---------------------
  annotation_values_manifest <- check_annotation_values(
    data$file_data[manifest_index][[1]],
    annotations,
    success_msg = "All values in the manifest are valid",
    fail_msg = "Some values in the manifest are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_indiv <- check_annotation_values(
    data$file_data[indiv_index][[1]],
    annotations,
    allowlist_keys = c("individualID"),
    success_msg = "All values in the individual metadata are valid",
    fail_msg = "Some values in the individual metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_biosp <- check_annotation_values(
    data$file_data[biosp_index][[1]],
    annotations,
    allowlist_keys = c("specimenID", "individualID"),
    success_msg = "All values in the biospecimen metadata are valid",
    fail_msg = "Some values in the biospecimen metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )
  annotation_values_assay <- check_annotation_values(
    data$file_data[assay_index][[1]],
    annotations,
    allowlist_keys = c("specimenID"),
    success_msg = "All values in the assay metadata are valid",
    fail_msg = "Some values in the assay metadata are invalid",
    annots_link = get_golem_config("annotations_link")
  )

  # Individual and specimen IDs are not duplicated ---------------------------
  duplicate_indiv_ids <- check_indiv_ids_dup(
    data$file_data[indiv_index][[1]],
    # nolint start
    success_msg = "Individual IDs in the individual metadata file are unique",
    fail_msg = "Duplicate individual IDs found in the individual metadata file"
    # nolint end
  )
  duplicate_specimen_ids <- check_specimen_ids_dup(
    data$file_data[biosp_index][[1]],
    # nolint start
    success_msg = "Specimen IDs in the biospecimen metadata file are unique",
    fail_msg = "Duplicate specimen IDs found in the biospecimen metadata file"
    # nolint end
  )

  # Empty columns produce warnings -------------------------------------------
  empty_cols_manifest <- check_cols_empty(
    data$file_data[manifest_index][[1]],
    required_cols = get_golem_config("complete_columns")$manifest,
    success_msg = "No columns are empty in the manifest",
    fail_msg = "Some columns are completely empty in the manifest"
  )
  empty_cols_indiv <- check_cols_empty(
    data$file_data[indiv_index][[1]],
    required_cols = get_golem_config("complete_columns")$individual,
    success_msg = "No columns are empty in the individual metadata",
    fail_msg = "Some columns are completely empty in the individual metadata"
  )
  empty_cols_biosp <- check_cols_empty(
    data$file_data[biosp_index][[1]],
    required_cols = get_golem_config("complete_columns")$biospecimen,
    success_msg = "No columns are empty in the biospecimen metadata",
    fail_msg = "Some columns are completely empty in the biospecimen metadata"
  )
  empty_cols_assay <- check_cols_empty(
    data$file_data[assay_index][[1]],
    required_cols = get_golem_config("complete_columns")$assay,
    success_msg = "No columns are empty in the assay metadata",
    fail_msg = "Some columns are completely empty in the assay metadata"
  )

  # Incomplete required columns produce failures -----------------------------
  complete_cols_manifest <- check_cols_complete(
    data$file_data[manifest_index][[1]],
    required_cols = get_golem_config("complete_columns")$manifest,
    success_msg = "There is no missing data in columns that are required to be complete in the manifest", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the manifest" # nolint
  )
  complete_cols_indiv <- check_cols_complete(
    data$file_data[indiv_index][[1]],
    required_cols = get_golem_config("complete_columns")$individual,
    success_msg = "There is no missing data in columns that are required to be complete in the individual metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the individual metadata" # nolint
  )
  complete_cols_biosp <- check_cols_complete(
    data$file_data[biosp_index][[1]],
    required_cols = get_golem_config("complete_columns")$biospecimen,
    success_msg = "There is no missing data in columns that are required to be complete in the biospecimen metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the biospecimen metadata" # nolint
  )
  complete_cols_assay <- check_cols_complete(
    data$file_data[assay_index][[1]],
    required_cols = get_golem_config("complete_columns")$assay,
    success_msg = "There is no missing data in columns that are required to be complete in the assay metadata", # nolint
    fail_msg = "There is missing data in some columns that are required to be complete in the assay metadata" # nolint
  )

  # Metadata files appear in manifest ----------------------------------------
  meta_files_in_manifest <- check_files_manifest(
    data$file_data[manifest_index][[1]],
    c(
      data$name[indiv_index],
      data$name[biosp_index],
      data$name[assay_index]
    ),
    success_msg = "Manifest file contains all metadata files",
    fail_msg = "Manifest file does not contain all metadata files"
  )

  # Parent column in manifest is valid synID -----------------------------------
  valid_parent_syn <- check_parent_syn(data$file_data[manifest_index][[1]])

  # Ages over 90 are censored in human individual metadata ---------------------
  if (any(data$species == "human", na.rm = TRUE)) {
    ages_over_90_indiv <- check_ages_over_90(
      data$file_data[indiv_index][[1]],
      success_msg = "No ages over 90 detected in the individual metadata",
      fail_msg = "Ages over 90 detected in the individual metadata"
    )
    ages_over_90_biosp <- check_ages_over_90(
      data$file_data[biosp_index][[1]],
      col = "samplingAge",
      success_msg = "No ages over 90 detected in the biospecimen metadata",
      fail_msg = "Ages over 90 detected in the biospecimen metadata"
    )
  } else {
    ages_over_90_biosp <- ages_over_90_indiv <- NULL
  }

  # No file paths are duplicated in the manifest -------------------------------
  duplicate_file_paths <- check_duplicate_paths(
    data$file_data[manifest_index][[1]]
  )

  # Additions to existing studies have complete IDs ----------------------------
  samples_table <- syn$tableQuery(
    glue::glue("SELECT * FROM {get_golem_config('samples_table')} WHERE study = '{study}'"), # nolint
    includeRowIdAndRowVersion = FALSE
  )
  samples_table <- readr::read_csv(samples_table$filepath)
  if (study %in% samples_table$study) {
    assay <- data[assay_index, "assay", drop = TRUE]
    complete_ids_indiv <- check_complete_ids(
      data$file_data[indiv_index][[1]],
      samples_table = samples_table,
      study = study,
      id_type = "individualID",
      success_msg = "All pre-existing individual IDs are present in the individual file", # nolint
      fail_msg = "Some individual IDs that were previously part of this study are missing from the individual file" # nolint
    )
    complete_ids_biosp <- check_complete_ids(
      data$file_data[biosp_index][[1]],
      samples_table = samples_table,
      study = study,
      id_type = "specimenID",
      success_msg = "All pre-existing specimen IDs are present in the biospecimen file", # nolint
      fail_msg = "Some specimen IDs that were previously part of this study are missing from the biospecimen file" # nolint
    )
    complete_ids_assay <- check_complete_ids(
      data$file_data[assay_index][[1]],
      samples_table = samples_table,
      study = study,
      id_type = "specimenID",
      assay = assay,
      success_msg = "All pre-existing specimen IDs for this assay are present in the assay file", # nolint
      fail_msg = "Some specimen IDs that were previously part of this study and assay are missing from the assay file" # nolint
    )
  } else {
    complete_ids_indiv <- complete_ids_biosp <- complete_ids_assay <- NULL
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
