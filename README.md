<!-- README.md is generated from README.Rmd. Please edit that file -->

# dccvalidator

[![Travis-CI Build
Status](https://travis-ci.org/Sage-Bionetworks/dccvalidator.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/dccvalidator)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

dccvalidator is a package and Shiny app to perform data validation and
QA/QC. It’s used in the [AMP-AD](https://ampadportal.org/) and
[PsychENCODE](http://www.psychencode.org) consortia to validate data
prior to data releases.

## Installation

``` r
devtools::install_github("Sage-Bionetworks/dccvalidator")
```

## Check data

dccvalidator provides functions for checking the following common data
quality issues:

  - Annotation keys and values conform to a controlled vocabulary
  - Column names match those of an associated metadata template
  - Certain columns are not empty
  - Certain columns are complete
  - Identifiers match between two metadata files (e.g. all individuals
    in one file are also present in another)
  - Check that identifiers are unique within a file

# Data submission validation

This package contains a Shiny app to validate manifests and metadata for
AMP-AD studies. It uses the dccvalidator package to check for common
data quality issues and gives realtime feedback to the data contributor
on errors that need to be fixed. The reporting UI is heavily inspired by
the [MetaDIG project’s metadata quality
reports](https://knb.ecoinformatics.org/quality/s=knb.suite.1/doi%3A10.5063%2FF12V2D1V).

The application also allows users to submit documentation of their
study, a description of the methods used, etc.

# Local development

`dccvalidator` uses pre-commit hooks to check for common issues, such as
code style (which should conform to [tidyverse
style](https://style.tidyverse.org/)), code parsability, and up-to-date
.Rd documentation. To use, you will need to install
[pre-commit](https://pre-commit.com/#intro). If on a Mac, I recommend
using [homebrew](https://brew.sh/):

    brew install pre-commit

Then, within this git repo, run:

    pre-commit install

When you commit your changes, pre-commit will run the checks described
above, and the commit will fail if the checks do not pass. If you are
experiencing issues with the checks and want to commit your work and
worry about them later, you can run `git commit --no-verify` to skip all
checks. Or, you can skip certain hooks by their ID (as shown in the file
`.pre-commit-config.yaml`), e.g. `SKIP=roxygenize git commit -m "foo"`.

-----

Please note that the dccvalidator project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
