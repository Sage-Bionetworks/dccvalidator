#' @title Get metadataType indices
#'
#' @description Get the metadataType indices for `data`.
#'
#' @export
#' @inheritParams check_all
#' @param meta_types List of metadataTypes to find.
#' @return List of indices for `data` where the metadataType exists;
#' `NULL` if none exist.
get_metadataType_indices <- function(data, meta_types) {
  if (!("metadataType" %in% colnames(data))) {
    stop("Missing metadataType.")
  }
  file_indices <- purrr::map(
    meta_types,
    function(x) {
      which(data$metadataType == x)
    }
  )
  names(file_indices) <- meta_types
  # Remove integer(0) values for indices
  no_indices <- purrr::map_lgl(file_indices, ~ length(.) < 1)
  file_indices <- file_indices[!no_indices]
  if (length(file_indices) > 0) {
    return(file_indices)
  } else {
    return(NULL)
  }
}

#' @title Gather all template ids
#'
#' @description Gather all template ids from config and add as a new column.
#'
#' @export
#' @param type The type of metadata template to get: manifest, individual,
#' biospecimen, or assay.
#' @param species The species needed to specify the correct biospecimen
#' or individual templates (default `NA`).
#' @param assay The assay needed to specify the correct assay template.
#' @returns the template id from the config (`NA` if not found).
gather_template_ids <- function(type, species = NA, assay = NA) {
  switch(type,
    manifest = get_golem_config("templates")$manifest_template,
    individual = gather_template_id_individual(species = species),
    biospecimen = gather_template_id_biospecimen(species = species),
    assay = gather_template_id_assay(assay = assay)
  )
}

## gather_template_ids helper
gather_template_id_individual <- function(species) {
  templates <- get_golem_config("templates")$individual_templates
  if (species %in% names(templates)) {
    return(templates[[species]])
  } else {
    return(NA)
  }
}

## gather_template_ids helper
gather_template_id_biospecimen <- function(species) {
  templates <- get_golem_config("templates")$biospecimen_templates
  if (species %in% names(templates)) {
    return(templates[[species]])
  } else {
    return(NA)
  }
}

## gather_template_ids helper
gather_template_id_assay <- function(assay) {
  templates <- get_golem_config("templates")$assay_templates
  if (assay %in% names(templates)) {
    return(templates[[assay]])
  } else {
    return(NA)
  }
}
