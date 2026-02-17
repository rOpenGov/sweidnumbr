# pin_birthplace_internal

Internal computation of birthplace (one for each pin)

## Usage

``` r
pin_birthplace_internal(pin, birth_vector, birth_other_text)
```

## Arguments

- pin:

  Character element with pin at standard format 'YYYYMMDDNNNC'. See
  [as.pin](https://ropengov.github.io/sweidnumbr/reference/as.pin.md).

- birth_vector:

  Vector mapping birth number to birthplace. See
  [pin_birthplace](https://ropengov.github.io/sweidnumbr/reference/pin_birthplace.md).

- birth_other_text:

  Text to return if born \>= 1990. See
  [pin_birthplace](https://ropengov.github.io/sweidnumbr/reference/pin_birthplace.md).

## Value

Character element containing birthplace
