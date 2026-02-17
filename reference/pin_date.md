# Calculate the date of birth from a `pin`

Calculates the date of birth in date format.

## Usage

``` r
pin_date(pin)
```

## Arguments

- pin:

  A vector of class `pin`. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

## Value

Date of birth as a vector in date format.

## Examples

``` r
# Examples taken from SKV 704 (see references)
ex_pin <- c("196408233234", "186408833224")
pin_date(ex_pin)
#> [1] "1964-08-23" "1864-08-23"
```
