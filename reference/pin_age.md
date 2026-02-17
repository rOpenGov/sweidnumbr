# Calculate age of `pin` for a given date

Calculate the age in full years for a given date.

## Usage

``` r
pin_age(pin, date = Sys.Date(), timespan = "years", verbose = TRUE)
```

## Arguments

- pin:

  A vector of class `pin`. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

- date:

  Date at which age is calculated. If a vector is provided it must be of
  the same length as the `pin` argument.

- timespan:

  Timespan to use to calculate age. The actual timespans are:

  - `years` (Default)

  - `months`

  - `weeks`

  - `days`

- verbose:

  Should messages be printed? Default is `TRUE`.

## Value

Age as an integer vector.

## References

- Skatteverket,
  [*Personnummer*](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv704-8.pdf).
  SKV 704. (2007)

## Examples

``` r
# Example with someone born today
today_pin <- 
  paste(paste(unlist(strsplit(as.character(Sys.Date()),split = "-")), collapse = ""),
        "0000",sep="")
pin_age(today_pin)
#> The age has been calculated at 2026-02-17.
#> [1] 0

# Examples taken from SKV 704 (see references)
ex_pin <- c("196408233234", "186408833224")
pin_age(ex_pin, date = "2012-01-01")
#> The age has been calculated at 2012-01-01.
#> [1]  47 147
```
