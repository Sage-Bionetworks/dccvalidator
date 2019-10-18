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

    #> Loading dccvalidator
    #> Loading required package: shinyBS

``` r
library("synapser")
library("dccvalidator")
synLogin()
```

``` r
## Get annotations
annots <- get_synapse_annotations()
#> 
 [####################]100.00%   1/1   Done...    
Downloading  [####################]100.00%   256.8kB/256.8kB (1.0MB/s) Job-98565045615138825278008857.csv Done...

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
Downloading  [####################]100.00%   3.2kB/3.2kB (1.4MB/s) Job-98565054187127989694636014.csv Done...
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
AMP-AD studies. It uses the dccvalidator package to check for common
data quality issues and gives realtime feedback to the data contributor
on errors that need to be fixed. The reporting UI is heavily inspired by
the [MetaDIG project’s metadata quality
reports](https://knb.ecoinformatics.org/quality/s=knb.suite.1/doi%3A10.5063%2FF12V2D1V).

## Deployment

This section is applicable to Sage employees who are deploying the
application on our Shiny Pro server. To learn about the server and how
to get credentials, please read the [Confluence
documentation](https://sagebionetworks.jira.com/wiki/spaces/SageShinyServer/pages/75497489/Shiny+Server).

The app is deployed on the server through the following steps:

1.  ssh into the Shiny Pro server and navigate to
    `/home/kwoo/ShinyApps/dccvalidator-app`
2.  `git pull` changes from GitHub
3.  To ensure packages are up-to-date, run `Rscript -e
    "renv::restore()"`

You may need to run `touch restart.txt` afterward to ensure the
application is restarted.

If you want to deploy the app in a different location (e.g. because you
want to stand up a new version that is customized for a different
community):

1.  ssh into the Shiny Pro server
2.  Create a folder under `/home/yourusername/ShinyApps`
3.  `git clone` the repository into the folder you’ve created
4.  If needed, make any changes to the application’s behavior by editing
    the files, or check out a branch that contains your changes
5.  To ensure packages are up-to-date, run `Rscript -e
    "renv::restore()"`

Again, you may need to run `touch restart.txt` afterward to ensure the
application is restarted.

## Local development

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
