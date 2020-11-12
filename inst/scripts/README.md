# Scripts

## update-metadata-template-dictionary.R

This script updates the dictionary sheets in existing metadata template files. A metadata template is expected to be an Excel (.xlsx) file with 3 sheets:
- template: header of metadata keys
- dictionary: definitions of the metadata keys present in the template (columns: key, description)
- values: enumerated values for the metadata keys present in the template (columns: key, value, valueDescription, source)

### Requirements

#### System

This script requires the dccvalidator package (version >= 0.3.0.9012). Install the latest version with:
```
remotes::install_github("Sage-Bionetworks/dccvalidator")
```

This script requires reticulate and the [Synapse Python
client](https://pypi.org/project/synapseclient/), as well. See the [reticulate
documentation](https://rstudio.github.io/reticulate/#python-version) for
information on how to set R to use a specific version of Python if you
don’t want to use the default Python installation on your machine.
Whichever Python installation you choose should have synapseclient
installed.

Finally, this script requires optparse, which is only a suggested package for dccvalidator. Please ensure this is installed by running:
```
install.packages("optparse")
```

#### config.yml
A configuration file is required in the working directory. The configuration needs, at minimum, a 'default' setting with the following options:
- `annotations_table`: Synapse ID of a table that lists allowable annotation keys and values for the consortium. This should follow the same basic format as our Synapse Annotations table, e.g. there must be the following columns: key, value, and columnType. columnType options are STRING, BOOLEAN, INTEGER, DOUBLE. This can be a list with multiple tables to pull annotations from. However, if a key appears more than once, it must have the same definition and columnType across all tables.
- `templates` (including `manifest_template`, `individual_templates`, `biospecimen_templates`, and `assay_templates`): Synapse IDs of templates to use for validation. These should be .xlsx files, where the column names reflect the required columns in the template.

### Running the Script

The script can be run with the following:
`Rscript <path to script>/update-metadata-template-dictionaries.R [options]`

The options are:
- `-u` `--username`: Synapse username or email. Required.
- `-p` `--password`: Synapse password. Defaults to `NA`.
- `-a` `--apikey`: Synapse apiKey. Defaults to `NA`.
- `-c` `--config`: Config.yml configuration setting. Defaults to `default`.
- `-d` `--directory`: Directory to download the templates to and remove from after updating.
Defaults to `.`.

The Synapse `username` is required, as well as one of the two other login keys, `password` or `apikey`.
