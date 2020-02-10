# Contributing to dccvalidator

This outlines how to propose a change to dccvalidator.

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a Git branch for each pull request (PR).  
*  Look at the Travis build status before and after making changes. The `README`
should contain badges for any continuous integration services used
by the package.  
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
You can use the [styler](https://CRAN.R-project.org/package=styler) package to
apply these styles, but please don't restyle code that has nothing to do with 
your PR.  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation.  
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.  

### Local development

`dccvalidator` uses pre-commit hooks to check for common issues, such as code
style (which should conform to [tidyverse style](https://style.tidyverse.org/)),
code parsability, and up-to-date .Rd documentation. To use, you will need to
install [pre-commit](https://pre-commit.com/#intro). If on a Mac, I recommend
using [homebrew](https://brew.sh/):

```
brew install pre-commit
```

Then, within this git repo, run:

```
pre-commit install
```

When you commit your changes, pre-commit will run the checks described above,
and the commit will fail if the checks do not pass. If you are experiencing
issues with the checks and want to commit your work and worry about them later,
you can run `git commit --no-verify` to skip all checks. Or, you can skip
certain hooks by their ID (as shown in the file `.pre-commit-config.yaml`), e.g.
`SKIP=roxygenize git commit -m "foo"`.

---

Please note that the dccvalidator project is released with a [Contributor Code of Conduct](https://sage-bionetworks.github.io/dccvalidator/CODE_OF_CONDUCT).
By contributing to this project, you agree to abide by its terms.


### Code of Conduct

Please note that the dccvalidator project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.


