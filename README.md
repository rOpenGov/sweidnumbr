<!-- badges: start -->
  [![R build status](https://github.com/rOpenGov/sweidnumbr/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/sweidnumbr/actions)
  [![Coverage Status](https://coveralls.io/repos/github/rOpenGov/sweidnumbr/badge.svg?branch=master)](https://coveralls.io/github/rOpenGov/sweidnumbr?branch=master) 
  [![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/sweidnumbr)](https://github.com/metacran/cranlogs.app)
  [![cran version](http://www.r-pkg.org/badges/version/sweidnumbr)](http://cran.rstudio.com/web/packages/sweidnumbr)
  <!-- badges: end -->

sweidnumbr
==========

## Introduction

`sweidnumbr` is an R package for structural handling of identity numbers used in the swedish administration such as personal identity numbers (personnummer) and organizational identity numbers (organisationsnummer).

## Installation

To install from CRAN just write:

```r
install.packages(sweidnumbr)
```

Use the `devtools` package to install the latest version from GitHub:
```r
devtools::install_github("rOpenGov/sweidnumbr")
library(sweidnumbr)
```

A tutorial is included with the package and can be viewed with:
```r
vignette("sweidnumbr")
```

## Reporting bugs

Please use the GitHub issue tracker [here](https://github.com/rOpenGov/sweidnumbr/issues) for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("sweidnumbr")`
