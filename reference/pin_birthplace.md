# Calculate the birthplace of `pin`

Calculate the birthplace for a given personal identity number born
before 1990. See details.

## Usage

``` r
pin_birthplace(pin)
```

## Arguments

- pin:

  A vector of class `pin`. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

## Value

Birthplace as factor.

## Details

It is possible to calculate where people where born (and/or if a person
has immigrated) through their personal identity number. This is possible
for people that was born before 1990 and after 1945.

For people born before 1946 the birthplace identifier contains
information on where one where registered the 1st of november 1946.

Personal identity numbers for people born after 1989 do not contain any
information on birthplace.

During the period 1946 - 1989 the pin also contains information on
whether one has immigrated to Sweden during the period.

## References

[SOU 2008:60 : Personnummer och
samordningsnummer](https://www.riksdagen.se/sv/dokument-och-lagar/dokument/statens-offentliga-utredningar/personnummer-och-samordningsnummer-del-1_gwb360/)

## Examples

``` r
# Example with someone born today and from SKV 704 (see references)
today_pin <- paste0(format(Sys.Date(),"%Y%m%d"), "0000")
ex_pin <- c("196408233234", today_pin)
pin_birthplace(ex_pin)
#> [1] Gotlands län                Born after 31 december 1989
#> 28 Levels: Stockholm stad Stockholms län Uppsala län ... Born after 31 december 1989
```
