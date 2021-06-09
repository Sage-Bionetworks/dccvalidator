# Contributing to dccvalidator

This outlines how to propose a change to dccvalidator.

### Fixing Typos

Small typos or grammatical errors in documentation may be edited directly using
the GitHub web interface, so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem or desired feature. If you’ve found a
bug, create an associated issue and illustrate the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex).

Once an issue exists, indicate that you plan on working on the update by commenting in the issue and, if able, assigning the issue to yourself.

### Contribution Process

All updates are done via pull request. We require that you create a Git branch for each pull request (PR). If you are unable to create a branch in the main repo, please see the section on forked repos below.  There is a default template for all pull requests, subject to change.

- **Fixes #:** Put the number of the issue (e.g. Fixes #123).
- **Changes proposed**: List the changes made.
- **Confirmation checklist**: This checklist is both a reminder and a verification that related items were done. If the item is relevant to the changes being made, then be sure to do the task associated. When the pull request includes the item, check the box by adding an 'x' (e.g. `- [x] `) if marking upon pull request creation or by clicking on the checkbox for existing pull requests. If the item is irrelevant to the changes being made, leave the checkbox unchecked. If the maintainers believe the item is relevant to the change, they will request an update to the pull request.

#### Guidelines

There are a few guidelines to make note of. Please see the Local Development section for more information, as well.

*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
   You can use the [styler](https://CRAN.R-project.org/package=styler) package to
   apply these styles, but please don't restyle code that has nothing to do with 
   your PR.  
*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with
   [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
   for documentation. 
*  We use [testthat](https://cran.r-project.org/package=testthat). Contributions
   with test cases included are easier to accept.
*  If adding or updating features, be aware that the maintainers may suggest breaking up a complicated function into two or more smaller, testable functions. This helps with testing, debugging, and reusability.

#### Forked Repos

If you do not have write access to the repo, please create a fork in your own account. Note that a pull request from a forked branch will fail the GitHub Actions tests due to not having the required secrets. Since the maintainers will need to spend extra time vetting the update, pull requests from forked repositories will take longer to process.

### Local Development

#### pre-commit hooks

`dccvalidator` uses pre-commit hooks to check for common issues, such as code
style (which should conform to [tidyverse style](https://style.tidyverse.org/)),
code parsability, and up-to-date .Rd documentation. To use, you will need to
install [pre-commit](https://pre-commit.com/#intro). If on a Mac, I recommend
using [homebrew](https://brew.sh/):

```R
brew install pre-commit
```

Then, within this git repo, run:

```R
pre-commit install
```

When you commit your changes, pre-commit will run the checks described above,
and the commit will fail if the checks do not pass. If you are experiencing
issues with the checks and want to commit your work and worry about them later,
you can run `git commit --no-verify` to skip all checks. Or, you can skip
certain hooks by their ID (as shown in the file `.pre-commit-config.yaml`), e.g.
`SKIP=roxygenize git commit -m "foo"`.

#### Manual checks

While pre-commit hooks are recommended, it's possible to verify that the update follows our standards.

##### Package Check

Install the `devtools` package and run the `check()` function. All checks should pass, including tests.

```R
devtools::check()
```

##### Style

As mentioned above, this package follows the [tidyverse style guide](http://style.tidyverse.org). Sticking to a single style helps with readability and allows for focusing on the code itself. Your code can be updated to the proper style by installing the `styler` package and running one of the styling functions on your code, such as `style_dir()`.

```R
styler::style_dir(file_type = "r")
```

Additionally, we also follow linting guidelines. While `styler` will enforce some of these guidelines, it is also useful to install `lintr` and run one of the linting functions, such as `lint_file()` or `lint_package()`. If there are linting errors, this will generate a list of problems to be fixed.

```
lintr::lint_package()
```

_Note: Only commit style and lint changes related to your own code. If there are style and lint changes unrelated to your code, these can be ignored._

### Code of Conduct

Please note that the dccvalidator project is released with a [Contributor Code of Conduct](https://sage-bionetworks.github.io/dccvalidator/CODE_OF_CONDUCT).
By contributing to this project, you agree to abide by its terms.

