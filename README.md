<!-- README.md is generated from README.Rmd. Please edit that file -->

# dccvalidator

[![Travis-CI Build
Status](https://travis-ci.org/Sage-Bionetworks/dccvalidator.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/dccvalidator)
[![Coverage
status](https://codecov.io/gh/Sage-Bionetworks/dccvalidator/branch/master/graph/badge.svg)](https://codecov.io/github/Sage-Bionetworks/dccvalidator?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Validate CNS data/metadata and annotations.

## Installation

``` r
devtools::install_github("Sage-Bionetworks/dccvalidator")
```

## Check annotations

You can check whether a Synapse file has valid annotation keys and
values. You can also check file views and data frames to see if their
columns correspond to valid annotations, and if the values in the
columns are valid.

``` r
library("synapser")
library("dccvalidator")
synLogin()
```

``` r
## Get annotations
annots <- get_synapse_annotations()

## File
my_file <- synGet("syn17038065", downloadFile = FALSE)
check_annotation_keys(my_file, annots)
#> <error>
#> message: Some annotation keys are invalid
#> class:   `check_fail`
check_annotation_values(my_file, annots)
#> <error>
#> message: Some annotation values are invalid
#> class:   `check_fail`

## File view
fv <- synTableQuery("SELECT * FROM syn17038067")
#> 
 [####################]100.00%   1/1   Done...    
Downloading  [####################]100.00%   3.2kB/3.2kB (1.9MB/s) Job-96628047720169821347303104.csv Done...
check_annotation_keys(fv, annots)
#> <error>
#> message: Some annotation keys are invalid
#> class:   `check_fail`
check_annotation_values(fv, annots)
#> <error>
#> message: Some annotation values are invalid
#> class:   `check_fail`

## Data frame
dat <- data.frame(assay = "foo", b = 2)
check_annotation_keys(dat, annots)
#> <error>
#> message: Some annotation keys are invalid
#> class:   `check_fail`
check_annotation_values(dat, annots)
#> <error>
#> message: Some annotation values are invalid
#> class:   `check_fail`
```

If you do not provide the annotations data frame to check against, the
functions will download the data automatically with
`get_synapse_annotations()`.

``` r
my_file <- synGet("syn17038065", downloadFile = FALSE)
check_annotation_keys(my_file)
#> <error>
#> message: Some annotation keys are invalid
#> class:   `check_fail`
check_annotation_values(my_file)
#> <error>
#> message: Some annotation values are invalid
#> class:   `check_fail`
```

The result of the `check_*()` functions is a custom condition object.
When the check returns an invalid result, the problematic data is
included.

``` r
dat <- data.frame(assay = "foo", b = 2)
res <- check_annotation_keys(dat)
res$data
#> [1] "b"
```

If you instead wish to view which annotations *are* valid, you can use
`valid_annotation_keys()` and `valid_annotation_values()`.

## Check template columns

You can check that the columns in your (meta)data conform to metadata
templates.

``` r
dat <- data.frame(specimenId = "a", assay = "rnaSeq")
check_cols_assay(dat, template = "rnaSeq")
#> <error>
#> message: Missing columns in the assay metadata file
#> class:   `check_fail`
```

## Check specimen and individual IDs

To make sure the specimen IDs or individual IDs match between files
(e.g. that the specimens described in a biospecimen file match those in
an assay metadata file), `check_specimen_ids_match()` and
`check_indiv_ids_match()`.

``` r
## Two data frames with different specimen IDs
a <- data.frame(specimenID = LETTERS[1:3])
b <- data.frame(specimenID = LETTERS[1:4])
(result <- check_specimen_ids_match(a, b, "biospecimen", "assay"))
#> <error>
#> message: specimenID values are mismatched between biospecimen and assay
#> class:   `check_fail`
result$data
#> $`Missing from biospecimen`
#> [1] "D"
#> 
#> $`Missing from assay`
#> character(0)
```

# Data submission validation

This package contains a Shiny app to validate manifests and metadata for
AMP-AD studies. It uses the
[dccvalidator](https://github.com/Sage-Bionetworks/dccvalidator) package
to check for common data quality issues and gives realtime feedback to
the data contributor on errors that need to be fixed. The reporting UI
is heavily inspired by the [MetaDIG project’s metadata quality
reports](https://knb.ecoinformatics.org/quality/s=knb.suite.1/doi%3A10.5063%2FF12V2D1V).

## Deployment

The app is deployed on the Sage Bionetworks’ Shiny Pro server through
the following steps:

1.  ssh into the Shiny Pro server and navigate to
    `/home/kwoo/ShinyApps/dccvalidator-app`
2.  `git pull` changes from GitHub
3.  To ensure packages are up-to-date, run `Rscript -e
    "renv::restore()"`

You may need to run `touch restart.txt` afterward to ensure the
application is restarted.

-----

Please note that the dccvalidator project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
