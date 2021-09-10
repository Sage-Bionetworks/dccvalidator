## Test environments

* macOS 10.13.6 High Sierra, R-release, CRAN setup (rhub)
* Windows Server 2008, R-release, 32/64 bit (winbuilder)
* Ubuntu 16.04 R 3.4 through R devel (travis-ci)
* local Ubuntu 18.04 R 3.6.3 build

## R CMD check results

0 errors | 0 warnings | 0 note

Added conditional requirement in json schema tests that will skip test if
`jsonvalidate` is not available. This is to avoid the problem with Fedora-clang
not having the V8 package.

## Downstream dependencies

There are currently no downstream dependencies for this package.
