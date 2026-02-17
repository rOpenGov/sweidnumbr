# Calculate sex from `pin`

Calculates the sex from the personal identification number.

## Usage

``` r
pin_sex(pin)
```

## Arguments

- pin:

  A vector of class `pin`. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

## Value

Factor with label 'Male' and 'Female'.

## References

- Skatteverket, [*Population registration in
  Sweden*](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv717B-4.pdf).
  SKV 717B. (2007)

- Skatteverket,
  [*Personnummer*](https://github.com/rOpenGov/sweidnumbr/blob/master/docs/skv704-8.pdf).
  SKV 704. (2007) [SOU 2008:60 : Personnummer och
  samordningsnummer](https://www.riksdagen.se/sv/dokument-och-lagar/dokument/statens-offentliga-utredningar/personnummer-och-samordningsnummer-del-1_gwb360/),
  (2008)

## Examples

``` r
# Examples taken from SKV 704 (see references)
ex_pin <- c("196408233234", "186408233224")
pin_sex(ex_pin)
#> [1] Male   Female
#> Levels: Female Male
```
