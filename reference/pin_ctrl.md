# Check control number from `pin`

Calculates the control number using the Luhn algorithm and compare it
with the control number in the personal identity number.

## Usage

``` r
pin_ctrl(pin, force_logical = FALSE)
```

## Arguments

- pin:

  A vector of class `pin`. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

- force_logical:

  If TRUE, force all NA in pin to be FALSE. Default is FALSE.

## Value

Logical vector indicating if a pin is correct (`TRUE`) or not (`FALSE`)

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
ex_pin <- c("196408233234", "196408233235")
pin_ctrl(ex_pin)
#> [1]  TRUE FALSE
```
