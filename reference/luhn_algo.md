# The Luhn algorithm

Calculates the control number for a Swedish personal/organisational
identity number using the Luhn algorithm.

## Usage

``` r
luhn_algo(id, multiplier)
```

## Arguments

- id:

  Element with swedish personal identity number.

- multiplier:

  What should each element in id be multiplied with

## Value

The control number (last digit in the personal identification number)
calculated from `id` (as integer).

## References

- [Luhn Algorithm](https://en.wikipedia.org/wiki/Luhn_algorithm).

- Skatteverket, [*Population registration in
  Sweden*](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv717B-4.pdf).
  SKV 717B. (2007)

- Skatteverket,
  [*Personnummer*](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv704-8.pdf).
  SKV 704. (2007)

## Examples

``` r
luhn_algo("121212121212", c(0,0,2,1,2,1,2,1,2,1,2,0))
#> [1] 2
luhn_algo(  "121212121",  c(      2,1,2,1,2,1,2,1,2))
#> [1] 2

## If no multiplier, the default is 
## to find one that match the format of id
luhn_algo("121212121212")
#> 'multiplier' set to: c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2, 0)
#> [1] 2
luhn_algo("12121212121")
#> 'multiplier' set to: c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2)
#> [1] 2
luhn_algo("1212121212")
#> 'multiplier' set to: c(2, 1, 2, 1, 2, 1, 2, 1, 2, 0)
#> [1] 2
luhn_algo("121212121")
#> 'multiplier' set to: c(2, 1, 2, 1, 2, 1, 2, 1, 2)
#> [1] 2

## Also for multiple pin 
## (as long they are all of the same format)
luhn_algo(c("12121212121", "19850504333"))
#> 'multiplier' set to: c(0, 0, 2, 1, 2, 1, 2, 1, 2, 1, 2)
#> [1] 2 4
if (FALSE) { # \dontrun{
try(luhn_algo(c("12121212121", "850504333"))) ## Different formats should fail!
} # }
```
