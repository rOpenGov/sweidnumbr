# sweidnumbr

  

## Introduction

`sweidnumbr` is an R package for structural handling of identity numbers
used in the swedish administration such as personal identity numbers
(personnummer) and organizational identity numbers
(organisationsnummer). For a quick introduction on how to use the
package, see the
[vignette](https://htmlpreview.github.io/?https://cran.r-project.org/web/packages/sweidnumbr/vignettes/sweidnumbr.html).

## Installation

To install from CRAN just write:

``` R
install.packages(sweidnumbr)
```

Use the `devtools` package to install the latest version from GitHub:

``` R
devtools::install_github("rOpenGov/sweidnumbr")
library(sweidnumbr)
```

A tutorial is included with the package and can be viewed with:

``` R
vignette("sweidnumbr")
```

## Reporting bugs

Please use the GitHub issue tracker
[here](https://github.com/rOpenGov/sweidnumbr/issues) for reporting bugs
and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the
developers easier by submitting the following information along with
your bug report:

- The output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
- The output of `packageVersion("sweidnumbr")`
