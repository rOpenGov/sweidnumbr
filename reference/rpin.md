# Generate a vector of random `pin`

A function that generates random `pin`s (see
[`as.pin`](https://ropengov.github.io/sweidnumbr/reference/as.pin.md)).
The generated `pin` is uniformely distributed over the time period.

## Usage

``` r
rpin(
  n,
  start_date = "1900-01-01",
  end_date = Sys.Date(),
  p.male = 0.5,
  p.coordn = 0.1
)
```

## Arguments

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

- start_date:

  Smallest possible `pin`. Default is 1900-01-01.

- end_date:

  Largest possible `pin`. Default is the current date.

- p.male:

  Proportion of males. Default is 0.5.

- p.coordn:

  Proportion of coordination numbers. Default is 0.1.

## Value

a vector of generated `pin`s.

## Examples

``` r
x <- rpin(3)
pin_ctrl(x)
#> [1] TRUE TRUE TRUE
pin_sex(x)
#> [1] Female Female Male  
#> Levels: Female Male
pin_age(x)
#> The age has been calculated at 2026-07-17.
#> [1]  45  42 114
```
