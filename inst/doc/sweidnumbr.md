<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{sweidnumbr}
-->

sweidnumbr : Structural handling of swedish identity numbers
===========

This R package provides tools to work with swedish identity numbers such as personal
identity numbers (personnummer) and organizational identity numbers (organisationsnummer).

## Table of contents

[Installation](#installation) (Installation)  

[Example: personal identity numbers](#examplepin) (Personal identity numbers)

[Example: organizational identity numbers](#exampleoin) (Organizational identity number)

[Licensing and Citations](#licens) (Licensing and Citations)

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

## <a name="examplepin"></a>Example: personal identity numbers

As a first step we need to convert personal identity numbers (pin) to the sam standard ABS format.


```r
example_pin <- c("640823-3234", "6408233234", "19640883-3230", "20080710-1023")
example_pin <- as.pin(example_pin)
```

```
## Assumption: 
## pin of format YYMMDDNNNC is assumed to be less than 100 years old.
```

```r
example_pin
```

```
## [1] "196408233234" "196408233234" "196408833230" "200807101023"
## Personal identity number(s)
```

To change a 'pin' follows ordinary R handling of vectors:


```r
example_pin[1] <- "20080710-1023"
example_pin
```

```
## [1] "200807101023" "196408233234" "196408833230" "200807101023"
## Personal identity number(s)
```

The next step is to test if the format is correct. To do this we use the ```is_pin()``` function.


```r
is.pin(example_pin)
```

```
## [1] TRUE
```

This only check the format of the pin. To check the pin using the control number we use ```pin_ctrl()```.


```r
pin_ctrl(example_pin)
```

```
## [1] FALSE  TRUE FALSE FALSE
```

We can now use ```pin_birthplace()``` and ```pin_sex()```. To get information on sex and birthplace.


```r
pin_sex(example_pin)
```

```
## [1] Female Male   Male   Female
## Levels: Female Male
```

```r
pin_birthplace(example_pin)
```

```
## [1] Born after 31 december 1989 Gotlands län               
## [3] <NA>                        Born after 31 december 1989
## 28 Levels: Stockholm stad Stockholms län Uppsala län ... Born after 31 december 1989
```

Use ```pin_coordn()``` to check if it is a coordination number.


```r
pin_coordn(example_pin)
```

```
## [1] FALSE FALSE  TRUE FALSE
```

As the last step we can calculate the age based on the pin. We choose the date where we want to calculate the age. If date is not specified the current date is used.


```r
pin_age(example_pin)
```

```
## The age has been calculated at 2015-06-07.
```

```
## [1]  6 50 50  6
```

```r
pin_age(example_pin, date = "2000-01-01")
```

```
## The age has been calculated at 2000-01-01.
```

```
## Warning: Negative age(es).
```

```
## [1] -9 35 35 -9
```

## <a name="exampleoin"></a>Example: organizational identity numbers

Handling of organizational identity numbers is done in a similar fashion. But organizational numbers are only allowed to have one format.


```r
example_oin <- c("556000-4615", "232100-0156", "802002-4280")
example_oin <- as.oin(example_oin)
example_oin
```

```
## [1] "556000-4615" "232100-0156" "802002-4280"
## Organizational identity number(s)
```

```r
example_oin[3] <- "556000-4615"
example_oin
```

```
## [1] "556000-4615" "232100-0156" "556000-4615"
## Organizational identity number(s)
```

We can test if the vector has a correct format in a similar way as for `pin`.


```r
is.oin(example_oin)
```

```
## [1] TRUE
```

With a vector of `oin` we can check if the control number is correct.


```r
oin_ctrl(example_oin)
```

```
## [1] TRUE TRUE TRUE
```

We can also check the type of organization. 


```r
oin_group(example_oin)
```

```
## [1] Aktiebolag                             
## [2] Stat, landsting, kommuner, församlingar
## [3] Aktiebolag                             
## Levels: Aktiebolag Stat, landsting, kommuner, församlingar
```

## <a name="licens"></a>Licensing and Citations

This work can be freely used, modified and distributed under the open license specified in the [DESCRIPTION file](https://github.com/rOpenGov/sweidnumbr/blob/master/DESCRIPTION).

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
##   http://github.com/rOpenGov/sweidnumbr
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
- [SKV 704 : Personnummer](https://www.skatteverket.se/download/18.1e6d5f87115319ffba380001857/1285595720207/70408.pdf)
- [SOU 2008:60 : Personnummer och samordningsnummer](http://www.riksdagen.se/sv/Dokument-Lagar/Utredningar/Statens-offentliga-utredningar/Personnummer-och-samordningsnu_GWB360/)
- [Protected personal identity numbers](https://www.skatteverket.se/foretagorganisationer/myndigheter/aviseringavbefolkningsuppgifternavet/skyddadepersonuppgifter.4.18e1b10334ebe8bc80001399.html)
- [SKV 709 : Organisationsnummer](http://www.skatteverket.se/download/18.70ac421612e2a997f85800040284/1359707510840/70909.pdf)

## Session info

This vignette was created with


```r
sessionInfo()
```

```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] sv_SE.UTF-8/sv_SE.UTF-8/sv_SE.UTF-8/C/sv_SE.UTF-8/sv_SE.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] sweidnumbr_0.5.0
## 
## loaded via a namespace (and not attached):
##  [1] digest_0.6.4         evaluate_0.5.5       formatR_0.10        
##  [4] htmltools_0.2.6      knitr_1.6            lubridate_1.4.0.9500
##  [7] rmarkdown_0.3.10     stringr_0.6.2        tools_3.1.2         
## [10] yaml_2.1.13
```
