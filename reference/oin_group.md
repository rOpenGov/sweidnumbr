# Calculate organization group from `oin`

Calculates the organization group from the organization number.

## Usage

``` r
oin_group(oin)
```

## Arguments

- oin:

  A vector of class `oin`. See
  [as.oin](https://ropengov.github.io/sweidnumbr/reference/as.oin.md).

## Value

Factor with organization categories.

## References

[Organisationsnummer
Skatteverket](http://www.skatteverket.se/foretagochorganisationer/foretagare/startaochregistrera/organisationsnummer.4.361dc8c15312eff6fd235d1.html?q=organisationsnummer)

## Examples

``` r
ex_oin <- c("556000-4615", "232100-0156", "802002-4280")
oin_group(ex_oin)
#> [1] Aktiebolag                             
#> [2] Stat, landsting, kommuner, församlingar
#> [3] Ideella föreningar och stiftelser      
#> 3 Levels: Aktiebolag ... Stat, landsting, kommuner, församlingar
```
