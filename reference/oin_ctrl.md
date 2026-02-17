# Check the control numbers for `oin`

Calculates the control number using the Luhn algorithm and compare it
with the control number in the organization identity number (oin).

## Usage

``` r
oin_ctrl(oin, force_logical = FALSE)
```

## Arguments

- oin:

  A vector of class `oin`. See
  [as.oin](https://ropengov.github.io/sweidnumbr/reference/as.oin.md).

- force_logical:

  If TRUE, force all NA in oin to be FALSE. Default is FALSE.

## Value

Logical vector indicating if a oin is correct (`TRUE`) or not (`FALSE`)

## References

[Organisationsnummer
Skatteverket](http://www.skatteverket.se/foretagochorganisationer/foretagare/startaochregistrera/organisationsnummer.4.361dc8c15312eff6fd235d1.html?q=organisationsnummer)

## Examples

``` r
ex_oin <- c("556000-4615", "232100-0156", "802002-4280", "232100-0157", "802002-4281")
oin_ctrl(ex_oin)
#> [1]  TRUE  TRUE  TRUE FALSE FALSE
```
