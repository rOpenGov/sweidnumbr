# Generate a vector of random `oin`

A function that generates random `oin`s (see
[`as.pin`](https://ropengov.github.io/sweidnumbr/reference/as.pin.md)).
The generated `oin` is uniformely distributed over all possible `oin`s.

## Usage

``` r
roin(n)
```

## Arguments

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

## Value

a vector of generated `oin`s.

## Examples

``` r
x <- roin(3)
oin_ctrl(x)
#> [1] TRUE TRUE TRUE
oin_group(x)
#> [1] Dödsbo                                 
#> [2] Ekonomiska föreningar                  
#> [3] Stat, landsting, kommuner, församlingar
#> 3 Levels: Dödsbo ... Stat, landsting, kommuner, församlingar
```
