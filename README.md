[![Build Status](https://travis-ci.org/MansMeg/sweidnumbr.svg)](https://travis-ci.org/MansMeg/sweidnumbr)

sweidnumbr
==========

## Introduction

sweidnumbr is an R package for structural handling of identity numbers used in the swedish administration such as personal identity numbers (personnummer) and organizational identity numbers (organisationsnummer).

## Installation

Use the `devtools` package to install the latest version:
```r
library("devtools")
devtools::install_github("sweidnumbr","MansMeg")
library(sweidnumbr)
```

A tutorial is included with the package and can be viewed with:
```r
vignette("sweidnumbr")
```

## Reporting bugs

Please use the GitHub issue tracker for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("sweidnumbr")`
