###############################################
#### Update Metadata Template Dictionaries ####
###############################################

if (!require("dccvalidator")) {
  remotes::install_github("Sage-Bionetworks/dccvalidator")
}
library("dccvalidator")
# magrittr is dccvalidator dependency
library("magrittr")

option_list <- list(
  optparse::make_option(
    c("-u", "--username"),
    type = "character",
    help = "Synapse username or email"
  ),
  optparse::make_option(
    c("-p", "--password"),
    type = "character",
    default = NA,
    help = "Synapse password"
  ),
  optparse::make_option(
    c("-a", "--apikey"),
    type = "character",
    default = NA,
    help = "Synapse apikey"
  ),
  optparse::make_option(
    c("-c", "--config"),
    type = "character",
    default = "default",
    help = "The configuration to use [default = %default]"
  ),
  optparse::make_option(
    c("-d", "--directory"),
    type = "character",
    default = ".",
    help = "output file name [default = %default]"
  )
)

opt_parser <- optparse::OptionParser(option_list = option_list);
opt <- optparse::parse_args(opt_parser);

## If running within R (not from command line), use opt list below with your
## needed parameters
# opt <- list(
#   config = "amp-ad",
#   directory = "templates",
#   username = NA,
#   password = NA,
#   apikey = NA
# )

## Set the configuration to use
## Change "default" to the appropriate configuration
Sys.setenv(R_CONFIG_ACTIVE = opt$config)

## Set up Synapse client
synapse <- reticulate::import("synapseclient")
syn <- synapse$Synapse()

## Log into Synapse
if (!is.na(opt$password)) {
  syn$login(
    email = opt$username,
    password = opt$password
  )
} else if (!is.na(opt$apikey)) {
  syn$login(
    email = opt$username,
    apiKey = opt$apikey
  )
}

## Get annotations
# Get all annotations and remove any duplicate rows
annots <- purrr::map_dfr(
  config::get("annotations_table"),
  get_synapse_annotations,
  syn = syn
) %>% dplyr::distinct()
## Verify annotations are in correct format
valid_results <- verify_dictionary_structure(annots)
## Check verification results
if (is.logical(valid_results)) {
  stop("Annotations failed verification.")
}
if (inherits(valid_results, "check_fail")) {
  stop(valid_results$message)
} # else assume valid structure

## Get all templates as vector of synIDs
temps <- unique(get_template_synIDs())

## Download and update template files with new dictionary sheets
updated_excel_files <- update_template_dictionaries(
  templates = temps,
  annotations = annots,
  syn = syn,
  directory = opt$directory
)

## Upload new versions of templates to Synapse
purrr::walk(
  updated_excel_files,
  function(x) {
    syn$store(x, forceVersion = FALSE)
  }
)

## Clean up local files
## Should be same directory for update_template_dictionaries (default = ".")
file.remove(
  list.files(
    opt$directory,
    pattern = "^template(.+)\\.xlsx",
    full.names = TRUE
  )
)
