<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{pxweb}
-->

sweidnumbr : Structural handling of swedish identity numbers
===========

This R package provides tools to work with swedish identity numbers such as personal
identity numbers (personnummer) and organizational identity numbers (organisationsnummer).

## Table of contents

[Installation](#installation) (Installation)  
[Examples](#examples) (Examples)  
[References](#references) (References)  

## <a name="installation"></a>Installation

Install the stable release version in R:


```r
install.packages("sweidnumbr")
```

Test the installation by loading the library:


```r
library(sweidnumbr)
```

We also recommend setting the UTF-8 encoding:


```r
Sys.setlocale(locale="UTF-8") 
```

## <a name="examples"></a>Examples

As a first step we need to convert personal identity numbers (pin) to the sam standard ABS format.


```r
example_pin <- c("640823-3234", "6408233234", "19640823-3230")
example_pin <- pin_format(example_pin)
example_pin
```

```
## [1] "196408233234" "196408233234" "196408233230"
```

The next step is to test if the format is correct. To do this we use the ```is_pin()``` function.


```r
is.pin(example_pin)
```

```
## [1] TRUE TRUE TRUE
```

This only check the format of the pin. To check the pin using the control number we use ```pin_ctrl()```.


```r
pin_ctrl(example_pin)
```

```
## [1]  TRUE  TRUE FALSE
```

We can now use ```pin_birthplace()``` and ```pin_sex()```. To get information on sex and birthplace.


```r
pin_sex(example_pin)
```

```
## [1] Male Male Male
## Levels: Male
```

```r
pin_birthplace(example_pin)
```

```
## [1] Gotlands län Gotlands län Gotlands län
## Levels: Gotlands län
```

As the last step we can calculate the age based on the pin. We choose the date where we want to calculate the age. If date is not specified the current date is used.


```r
pin_age(example_pin)
```

```
## [1] 50 50 50
```

```r
pin_age(example_pin, date = "2000-01-01")
```

```
## [1] 35 35 35
```


## Licensing and Citations

This work can be freely used, modified and distributed under the open license specified in the [DESCRIPTION file](https://github.com/MansMeg/sweidnumbr/blob/master/DESCRIPTION).

Kindly cite the work as follows


```r
citation("sweidnumbr")
```

```
## 
## Kindly cite the sweidnumbr R package as follows:
## 
##   (C) Mans Magnusson(2014).  sweidnumbr: R tools to handle of
##   swedish identity numbers. URL:
##   http://github.com/MansMeg/sweidnumbr
## 
## A BibTeX entry for LaTeX users is
## 
##   @Misc{,
##     title = {sweidnumbr: R tools to handle of swedish identity numbers.},
##     author = {Mans Magnusson},
##     year = {2014},
##   }
```


## <a name="references"></a>References 

- [Population registration in Sweden](https://www.skatteverket.se/download/18.8dcbbe4142d38302d74be9/1387372677724/717B06.pdf)
- [SKV 704](https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf)
- [SOU 2008:60 : Personnummer och samordningsnummer](http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/)


## Session info

This vignette was created with


```r
sessionInfo()
```

```
## R version 3.1.0 (2014-04-10)
## Platform: x86_64-apple-darwin13.1.0 (64-bit)
## 
## locale:
## [1] sv_SE.UTF-8/sv_SE.UTF-8/sv_SE.UTF-8/C/sv_SE.UTF-8/sv_SE.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lubridate_1.3.3 sweidnumbr_0.1 
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.4     evaluate_0.5.5   formatR_0.10     htmltools_0.2.4 
##  [5] knitr_1.6        memoise_0.2.1    plyr_1.8.1       Rcpp_0.11.1     
##  [9] rmarkdown_0.2.64 stringr_0.6.2    tools_3.1.0      yaml_2.1.13
```
