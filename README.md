<!-- README.md is generated from README.Rmd. Please edit that file -->

# dccvalidator

[![Travis-CI Build
Status](https://travis-ci.org/Sage-Bionetworks/dccvalidator.svg?branch=master)](https://travis-ci.org/Sage-Bionetworks/dccvalidator)
[![Coverage
status](https://codecov.io/gh/Sage-Bionetworks/dccvalidator/branch/master/graph/badge.svg)](https://codecov.io/github/Sage-Bionetworks/dccvalidator?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

Validate CNS Data and Metadata

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
#> Invalid keys:
#> randomAnnotation
check_annotation_values(my_file, annots)
#> Invalid values:
#> species: "wrongSpecies"
#> assay: "wrongAssay"

## File view
fv <- synTableQuery("SELECT * FROM syn17038067")
#> 
 [####################]100.00%   1/1   Done...    
Downloading  [####################]100.00%   2.7kB/2.7kB (941.0kB/s) Job-80532951533726281596216641.csv Done...
check_annotation_keys(fv, annots)
#> Invalid keys:
#> randomAnnotation
check_annotation_values(fv, annots)
#> Invalid values:
#> assay: "wrongAssay"
#> species: "wrongSpecies"

## Data frame
dat <- data.frame(assay = "foo", b = 2)
check_annotation_keys(dat, annots)
#> Invalid keys:
#> b
check_annotation_values(dat, annots)
#> Invalid values:
#> assay: "foo"
```

If you do not provide the annotations data frame to check against, the
functions will download the data automatically with
`get_synapse_annotations()`.

``` r
my_file <- synGet("syn17038065", downloadFile = FALSE)
check_annotation_keys(my_file)
#> Invalid keys:
#> randomAnnotation
check_annotation_values(my_file)
#> Invalid values:
#> species: "wrongSpecies"
#> assay: "wrongAssay"
```

If you instead wish to view which annotations *are* valid, you can use
`valid_annotation_keys()` and `valid_annotation_values()`.

-----

Please note that the dccvalidator project is released with a
[Contributor Code of Conduct](.github/CODE_OF_CONDUCT.md). By
contributing to this project, you agree to abide by its terms.
