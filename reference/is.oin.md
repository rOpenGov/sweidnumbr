# Test if a character vector contains correct `oin`

Test which elements in a text vector that contains organization identity
number.

## Usage

``` r
is.oin(oin)
```

## Arguments

- oin:

  Character vector to be tested if it is an `oin` of the right format.

## Value

Logical vector indicating if the elements can be an organization
identity number.

## Examples

``` r
ex_oin <- roin(3)
is.oin(ex_oin)
#> [1] TRUE

ex_oin_char <- as.character(ex_oin)
is.oin(ex_oin_char)
#> [1] FALSE
```
