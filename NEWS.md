# dccvalidator (development version)

- Add `check_complete_ids()` and `samples_table` configuration option
- Fix bug in Data Summary information boxes to not count NA
- `check_all()` now checks the biospecimen metadata for ages over 90 in the
  `samplingAge` column
- Results boxes now contain explanations of their contents
- The wording of some check results has been updated and, hopefully, clarified
- Add next step message if all validated files have no failures
- Add reset buttons to documentation and validation sections

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
