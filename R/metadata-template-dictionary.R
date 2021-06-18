#' @title Update Template Dictionaries
#'
#' @description This function acts as a script to update current metadata
#' template dictionaries. The metadata template will be an Excel file with three
#' sheets, template, dictionary, values. The template only has the required
#' column headers and all other rows are blank. This script assumes this part
#' exists within the Excel file. The dictionary sheet has two columns, key and
#' description, which has all the unique keys present in the template sheet and
#' their respective definitions. The values sheet has four columns, key, value,
#' valueDescription, and source. The keys are limited to those present in the
#' template sheet and values sheet contains the possible enumerated values for
#' those keys.
#'
#' @param templates A vector of metadata template synIDs.
#' @param annotations Data frame of annotation dictionary. Required to have
#' the columns: key, description, value, valueDescription, source.
#' @param syn Synapse client object.
#' @param directory The directory to download and save new versions of the
#' metadata templates to. Defaults to `"."`.
#' @return List of Synapse file objects, one for each metadata synID.
#'
#' @export
#' @examples
#' \dontrun{
#' # Fake templates and annotations
#' temps <- c("syn111111", "syn222222")
#' annots <- data.frame(
#'   key = c("first_name", "last_name", "last_name", "age"),
#'   description = c(
#'     "A person's given name",
#'     "A person's family name",
#'     "A person's family name",
#'     "A person's age"
#'   ),
#'   value = c(NA, "Smith", "Robinson", NA),
#'   valueDescription = c(
#'     NA,
#'     "From the Smith family",
#'     "From the Robinson family",
#'     NA
#'   ),
#'   source = c(NA, "Self-defined", "Self-defined", NA)
#' )
#'
#' # Get Synapse client object and login
#' synapse <- reticulate::import("synapseclient")
#' syn <- synapse$Synapse()
#' # This style of login only works with a Synapse config file
#' syn$Login()
#'
#' update_template_dictionaries(
#'   templates = temp,
#'   annotations = annots,
#'   syn = syn
#' )
#' }
update_template_dictionaries <- function(templates, annotations, syn,
                                         directory = ".") {
  purrr::map(templates, function(x) {
    local_file <- syn$get(x, downloadLocation = directory)
    updated <- add_dictionary_sheets(local_file$path, annotations = annotations)
    local_file
  })
}

#' @title Verify Dictionary Structure
#'
#' @description Checks that no key has more than one description
#' or columnType. Throws error if the dictionary is invalid.
#'
#' @param dictionary Dataframe with columns 'key', 'description',
#' and 'columnType'.
#'
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Valid
#' dat <- data.frame(
#'   key = c("first_name", "first_name"),
#'   description = c(
#'     "First name of individual",
#'     "First name of individual"
#'   ),
#'   columnType = c("string", "string")
#' )
#' verify_dictionary_structure(dat)
#'
#' # Invalid -- throws error due to descriptions of first_name
#' dat <- data.frame(
#'   key = c("first_name", "first_name"),
#'   description = c(
#'     "First name of individual",
#'     "Last name of individual"
#'   ),
#'   columnType = c("string", "string")
#' )
#' verify_dictionary_structure(dat)
#' }
verify_dictionary_structure <- function(dictionary) {
  # Check that dictionary exists and has needed elements
  if (!inherits(dictionary, "data.frame")) {
    stop("Dictionary not provided as a data frame")
  }
  if (!all(c("key", "description", "columnType") %in% colnames(dictionary))) {
    missing_cols <- setdiff(
      c("key", "description", "columnType"),
      colnames(dictionary)
    )
    # Make a readable string
    missing_cols <- glue::glue_collapse(missing_cols, sep = ", ")
    stop(
      glue::glue("Dictionary is missing the column(s): {missing_cols}")
    )
  }

  # Check that the dictionary is valid
  dict_summary <- dictionary %>%
    dplyr::group_by(.data$key) %>%
    dplyr::summarise(
      key = unique(.data$key),
      n_description = dplyr::n_distinct(.data$description),
      n_type = dplyr::n_distinct(.data$columnType)
    )

  # Errors if there's > 1 description for a key
  if (any(dict_summary$n_description > 1)) {
    dat <- dict_summary$key[dict_summary$n_description > 1]
    return(check_fail(
      msg = "At least one key had > 1 description",
      behavior = "All duplicate keys should have the same description",
      data = dat
    ))
  }

  # Errors if there's > 1 columnType for a key
  if (any(dict_summary$n_type > 1)) {
    dat <- dict_summary$key[dict_summary$n_type > 1]
    return(check_fail(
      msg = "At least one key had > 1 columnType",
      behavior = "All duplicate keys should have the same columnType",
      data = dat
    ))
  }

  return(check_pass(
    msg = "All keys had a single description and columnType",
    behavior = "All keys should have a single descrioption and columnType"
  ))
}

#' @title Add Dictionary Sheets
#'
#' @description Overwrites an excel file with three sheets: template,
#' dictionary, and values.
#'
#' @param template_xlsx_path Path to existing Excel template. Template
#' is expected to have at least the first sheet with template column headers.
#' @param annotations Data frame of annotation dictionary. Required to have
#' the columns: key, description, value, valueDescription, source.
#' @return template_xlsx_path
#'
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' # Annotations example
#' annots <- data.frame(
#'   key = c("first_name", "last_name", "last_name", "age"),
#'   description = c(
#'     "A person's given name",
#'     "A person's family name",
#'     "A person's family name",
#'     "A person's age"
#'   ),
#'   value = c(NA, "Smith", "Robinson", NA),
#'   valueDescription = c(
#'     NA,
#'     "From the Smith family",
#'     "From the Robinson family",
#'     NA
#'   ),
#'   source = c(NA, "Self-defined", "Self-defined", NA)
#' )
#'
#' # Need file with template sheet to exist
#' template_path <- "./my_template.xlsx"
#' template <- data.frame(
#'   first_name = NA,
#'   last_name = NA,
#'   age = NA
#' )
#' writexl::write_xlsx(template, template_path)
#'
#' # Add extra sheets and overwrite file
#' add_dictionary_sheets(template_path, annots)
#' }
add_dictionary_sheets <- function(template_xlsx_path, annotations) {
  required_cols <- c(
    "key",
    "description",
    "value",
    "valueDescription",
    "source"
  )
  if (!(all(required_cols %in% colnames(annotations)))) {
    missing_cols <- glue::glue_collapse(
      setdiff(required_cols, colnames(annotations)),
      sep = ", "
    )
    stop(glue::glue("Annotations are missing the column(s): {missing_cols}"))
  }
  template <- readxl::read_xlsx(template_xlsx_path, sheet = 1)

  ## Filter down to those present in the current template
  annots_filter <- dplyr::filter(annotations, .data$key %in% names(template))

  ## Dictionary of fields
  dictionary <- generate_key_description(annots_filter)

  ## Dictionary of values
  values <- annots_filter %>%
    dplyr::select(.data$key, .data$value, .data$valueDescription, .data$source)

  results <- list(
    template = template,
    dictionary = dictionary,
    values = values
  )

  writexl::write_xlsx(results, template_xlsx_path, format_headers = FALSE)
  return(template_xlsx_path)
}

#' @title Generate Key Description
#'
#' @description Generates a table with unique keys and their descriptions.
#'
#' @param annots Data frame with annotations. Required to have the columns 'key'
#' and 'description', and all duplicated keys must have the identical
#' descriptions.
#' @return Tibble data frame of unique keys and their descriptions.
#'
#' @noRd
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' annots <- data.frame(
#'   key = c("first_name", "last_name", "last_name"),
#'   description = c(
#'     "A person's given name",
#'     "A person's family name",
#'     "A person's family name"
#'   )
#' )
#' generate_key_description(annots)
#' }
generate_key_description <- function(annots) {
  if (!(all(c("key", "description") %in% colnames(annots)))) {
    stop("Annotations missing 'key' or 'description' columns.")
  }
  annots %>%
    dplyr::group_by(.data$key) %>%
    dplyr::summarize(
      key = unique(.data$key),
      description = unique(.data$description)
    )
}

#' @title Get Template synIDs
#'
#' @description Get a vector of template synIDs from either a list or from the
#' config.yml file.
#'
#' @param templates Named or unnamed list of template synIDs. Defaults to
#' `get_golem_config("templates")`.
#' @return A vector of synIDs.
#' @export
#' @importFrom magrittr %>%
#' @examples
#' \dontrun{
#' named_list <- dat1 <- list(
#'   template1 = "syn111111",
#'   template2 = "syn222222",
#'   template_set = list(
#'     template3 = "syn333333",
#'     template4 = "syn444444"
#'   )
#' )
#' get_template_synIDs(templates = named_list)
#'
#' # config.yml example
#' # Write config file; this has fake synIDs and will not function
#' config <- list(
#'   default = list(
#'     templates = list(
#'       template1 = "syn222222",
#'       template2 = "syn3333333"
#'     )
#'   )
#' )
#' yaml::write_yaml(config, "./config.yml")
#' get_template_synIDs()
#' }
get_template_synIDs <- function(templates = get_golem_config("templates")) {
  # Get all template synIDs as vector
  templates %>%
    purrr::flatten() %>%
    unname() %>%
    unlist()
}
