# sweidnumbr : Structural handling of swedish identity numbers

This R package provides tools to work with swedish identity numbers such
as personal identity numbers (personnummer) and organizational identity
numbers (organisationsnummer).

## Table of contents

[Installation](#installation) (Installation)

[Example: personal identity numbers](#examplepin) (Personal identity
numbers)

[Example: organizational identity numbers](#exampleoin) (Organizational
identity number)

[Licensing and Citations](#licens) (Licensing and Citations)

[References](#references) (References)

## Installation

Install the stable release version in R:

``` r
install.packages("sweidnumbr")
```

Test the installation by loading the library:

``` r
library(sweidnumbr)
```

We also recommend setting the UTF-8 encoding:

``` r
Sys.setlocale(locale="UTF-8") 
```

## Example: personal identity numbers

As a first step we need to convert personal identity numbers (pin) to
the same standard format used by the Swedish tax authority.

``` r
example_pin <- c("640823-3234", "6408233234", "19640823-3230")
example_pin <- as.pin(example_pin)
example_pin
```

    ## [1] "196408233234" "196408233234" "196408233230"
    ## Personal identity number(s)

The next step is to test if the vector is a `pin` object. To do this we
use the
[`is.pin()`](https://ropengov.github.io/sweidnumbr/reference/as.pin.md)
function.

``` r
is.pin(example_pin)
```

    ## [1] TRUE

This only check the format of the pin. To check the pin using the
control number we use
[`pin_ctrl()`](https://ropengov.github.io/sweidnumbr/reference/pin_ctrl.md).

``` r
pin_ctrl(example_pin)
```

    ## [1]  TRUE  TRUE FALSE

We can now use
[`pin_birthplace()`](https://ropengov.github.io/sweidnumbr/reference/pin_birthplace.md)
and
[`pin_sex()`](https://ropengov.github.io/sweidnumbr/reference/pin_sex.md).
To get information on sex and birthplace.

``` r
pin_sex(example_pin)
```

    ## [1] Male Male Male
    ## Levels: Male

``` r
pin_birthplace(example_pin)
```

    ## [1] Gotlands län Gotlands län Gotlands län
    ## 28 Levels: Stockholm stad Stockholms län Uppsala län ... Born after 31 december 1989

As the last step we can calculate the age based on the pin. We choose
the date where we want to calculate the age. If date is not specified
the current date is used.

``` r
pin_age(example_pin)
```

    ## [1] 61 61 61

``` r
pin_age(example_pin, date = "2000-01-01")
```

    ## [1] 35 35 35

It is also possible to format the pin for presentation in different
forms. (Note however that the output of `format_pin` is just a character
and no longer a `pin` object):

``` r
format_pin(example_pin, "%Y-%m-%d-%N")
```

    ## [1] "1964-08-23-3234" "1964-08-23-3234" "1964-08-23-3230"

``` r
format_pin(example_pin, "%P")
```

    ## [1] "(19) 64-08-23 - 3234" "(19) 64-08-23 - 3234" "(19) 64-08-23 - 3230"

Sometimes we want some example `pin`s. We can easily simulate `pin`s
using
[`rpin()`](https://ropengov.github.io/sweidnumbr/reference/rpin.md):

``` r
rpin(3)
```

    ## [1] "191003104971" "200504042896" "197510167344"
    ## Personal identity number(s)

## Example: organizational identity numbers

Handling of organizational identity numbers is done in a similar
fashion. But organizational numbers are only allowed to have one format.

``` r
example_oin <- c("556000-4615", "232100-0156", "802002-4280")
example_oin <- as.oin(example_oin)
example_oin
```

    ## [1] "556000-4615" "232100-0156" "802002-4280"
    ## Organizational identity number(s)

We can test if the vector has a correct format in a similar way as for
`pin`.

``` r
is.oin(example_oin)
```

    ## [1] TRUE

With a vector of `oin` we can check if the organizational number is
correct.

``` r
oin_ctrl(example_oin)
```

    ## [1] TRUE TRUE TRUE

We can also check the type of organization.

``` r
oin_group(example_oin)
```

    ## [1] Aktiebolag                             
    ## [2] Stat, landsting, kommuner, församlingar
    ## [3] Ideella föreningar och stiftelser      
    ## 3 Levels: Aktiebolag ... Stat, landsting, kommuner, församlingar

Sometimes we want some example `oin`s. We can easily simulate `oin`s
using
[`roin()`](https://ropengov.github.io/sweidnumbr/reference/roin.md):

``` r
roin(3)
```

    ## [1] "677427-9803" "617882-7413" "863567-0519"
    ## Organizational identity number(s)

## Licensing and Citations

This work can be freely used, modified and distributed under the open
license specified in the [DESCRIPTION
file](https://github.com/rOpenGov/sweidnumbr/blob/master/DESCRIPTION).

Kindly cite the work as follows

``` r
citation("sweidnumbr")
```

    ## Kindly cite the sweidnumbr R package as follows:
    ## 
    ##   Magnusson, Mans and Bulow, Erik (2024). sweidnumbr: R tools to handle
    ##   of swedish identity numbers. R package version 1.5.0 URL:
    ##   https://github.com/rOpenGov/sweidnumbr
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Misc{,
    ##     title = {sweidnumbr: R tools to handle of swedish identity numbers.},
    ##     author = {Mans Magnusson and Erik Bulow},
    ##     url = {https://github.com/rOpenGov/sweidnumbr},
    ##     year = {2024},
    ##     note = {R package version 1.5.0},
    ##   }

## References

- [Population registration in
  Sweden](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv717B-4.pdf).
  (2007)
- [SKV 704 :
  Personnummer](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv704-8.pdf).
  (2007)
- [SOU 2008:60 : Personnummer och
  samordningsnummer](https://www.riksdagen.se/sv/dokument-och-lagar/dokument/statens-offentliga-utredningar/personnummer-och-samordningsnummer-del-1_gwb360/).
  (2008)
- Personnummer: information fran Centrala folkbokförings- och
  uppbördsnämnden. (1967). Stockholm
- Den svenska folkbokföringens historia under tre sekel. (1982). Solna:
  Riksskatteverket
  [URL](http://www.skatteverket.se/privat/folkbokforing/omfolkbokforing/folkbokforingigaridag/densvenskafolkbokforingenshistoriaundertresekler.4.18e1b10334ebe8bc80004141.md)
- [Lag (1974:174) om identitetsbeteckning for juridiska personer
  m.fl.](https://www.riksdagen.se/sv/dokument-och-lagar/dokument/svensk-forfattningssamling/lag-1974174-om-identitetsbeteckning-for_sfs-1974-174/)

## Session info

This vignette was created with

``` r
sessionInfo()
```

    ## R version 4.5.2 (2025-10-31)
    ## Platform: x86_64-pc-linux-gnu
    ## Running under: Ubuntu 24.04.3 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    ## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
    ##  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
    ##  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
    ## [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
    ## 
    ## time zone: UTC
    ## tzcode source: system (glibc)
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] sweidnumbr_1.5.0
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] cli_3.6.5         knitr_1.51        rlang_1.1.7       xfun_0.56        
    ##  [5] stringi_1.8.7     generics_0.1.4    textshaping_1.0.4 jsonlite_2.0.0   
    ##  [9] glue_1.8.0        backports_1.5.0   htmltools_0.5.9   ragg_1.5.0       
    ## [13] sass_0.4.10       rmarkdown_2.30    evaluate_1.0.5    jquerylib_0.1.4  
    ## [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   stringr_1.6.0    
    ## [21] compiler_4.5.2    fs_1.6.6          timechange_0.4.0  htmlwidgets_1.6.4
    ## [25] systemfonts_1.3.1 digest_0.6.39     R6_2.6.1          magrittr_2.0.4   
    ## [29] checkmate_2.3.4   bslib_0.10.0      tools_4.5.2       lubridate_1.9.5  
    ## [33] pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
