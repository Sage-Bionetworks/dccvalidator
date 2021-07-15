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
#' @param biospecimen_type The type of biospecimen template needed
#' (default `NA`).
#' @returns the template id from the config (`NA` if not found).
gather_template_ids <- function(type, species = NA, assay = NA,
                                biospecimen_type = NA) {
  templates <- get_golem_config("templates")
  switch(type,
    manifest = templates[["manifest_template"]],
    individual = gather_template_id_individual(
      templates = templates,
      species = species
    ),
    biospecimen = gather_template_id_biospecimen(
      templates = templates,
      species = species,
      biospecimen_type = biospecimen_type
    ),
    assay = gather_template_id_assay(
      templates = templates,
      assay = assay
    )
  )
}

#' @title Grab specific biospecimen template ID
#'
#' @description Helper for [gather_template_ids]. Grabs the ID for a specific
#' biospecimen template based on the species and, if relevant, the biospecimen
#' type.
#'
#' @noRd
#' @param templates Named list of templates, `biospecimen_templates`,
#' `individual_templates`, `assay_templates`.
#' @inheritParams gather_template_ids
#' @return Biospecimen template ID from `templates` that matches the `species`
#' and `biospecimen_type`, if relevant, or `NA` if the biospecimen templates
#' had no match to the species and type.
gather_template_id_biospecimen <- function(templates, species,
                                           biospecimen_type) {
  if (species %in% names(templates[["biospecimen_templates"]])) {
    if (is.na(biospecimen_type) | biospecimen_type %in% "") {
      # Grab based on species
      return(templates[["biospecimen_templates"]][[species]])
    } else {
      # Grab based on both species and type
      return(
        templates[["biospecimen_templates"]][[species]][[biospecimen_type]]
      )
    }
  } else {
    return(NA)
  }
}

#' @title Grab specific individual template ID
#'
#' @description Helper for [gather_template_ids]. Grabs the ID for a specific
#' individual based on the species.
#'
#' @noRd
#' @inheritParams gather_template_id_biospecimen
#' @inheritParams gather_template_ids
#' @return Individual template ID from `templates` that matches the `species`,
#' or `NA` if there was no assay type that matched.
gather_template_id_individual <- function(templates, species) {
  if (species %in% names(templates[["individual_templates"]])) {
    return(templates[["individual_templates"]][[species]])
  } else {
    return(NA)
  }
}

#' @title Grab specific assay template ID
#'
#' @description Helper for [gather_template_ids]. Grabs the ID for a specific
#' assay template based on the assay type.
#'
#' @noRd
#' @inheritParams gather_template_id_biospecimen
#' @inheritParams gather_template_ids
#' @return Assay template ID from template that matches the `assay`, or `NA`
#' if there was no assay type that matched.
gather_template_id_assay <- function(templates, assay) {
  if (assay %in% names(templates[["assay_templates"]])) {
    return(templates[["assay_templates"]][[assay]])
  } else {
    return(NA)
  }
}
