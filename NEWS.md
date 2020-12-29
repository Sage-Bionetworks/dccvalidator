# dccvalidator (development version)

- Add `check_complete_ids()` and `samples_table` configuration option
- Fix bug in Data Summary information boxes to not count NA
- `check_all()` now checks the biospecimen metadata for ages over 90 in the
  `samplingAge` column
- Results boxes now contain explanations of their contents
- The wording of some check results has been updated and, hopefully, clarified
- Add next step message if all validated files have no failures
- Add reset buttons to documentation and validation sections
- All tooltips now pop-up with hover instead of clicking
- Add hover tooltips to all fileInputs
- Fix bug in behavior for `check_ages_over_90()` and `check_parent_syn()`
- Remove progress bars for file input boxes
- Add script and functions for updating metadata template dictionary sheets.
- `check_annotation_keys()` and `check_annotation_values()` have renamed
  arguments from `whitelist_keys`/`whitelist_values` to
  `allowlist_keys`/`allowlist_values`

# dccvalidator v0.3.0

- Add `check_parent_syn()`
- Add modal message if `sessionToken` is invalid
- Add `check_ages_over_90()`
- Add `check_duplicate_paths()`
- Update `can_coerce()` to be `TRUE` for any data type needing to be coerced to `character`

# dccvalidator v0.2.0

- Move all validation checks to `check_all()`
- Move data file summary to module
- Update `can_coerce()` to include numeric to integer and character to logical

# dccvalidator v0.1.0

Initial release of dccvalidator package.
