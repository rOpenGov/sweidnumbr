# Parse organizational identity numbers

Check and convert a vector of organizational identity numbers.

## Usage

``` r
as.oin(oin)
```

## Arguments

- oin:

  Vector with swedish organizational identity numbers in character
  format. See details.

## Value

Character vector (of class `oin` and `AsIs`) with swedish organizational
identity numbers.

## Details

The following format is accepted:

- character: `GNNNNN-NNNC`

## References

[Lag (1974:174) om identitetsbeteckning for juridiska personer
m.fl.](https://www.riksdagen.se/sv/dokument-och-lagar/dokument/svensk-forfattningssamling/lag-1974174-om-identitetsbeteckning-for_sfs-1974-174/)

## Examples

``` r
ex_oin <- c("556000-4615", "232100-0156", "802002-4280", "8020024280", "AA2002-4280")
as.oin(ex_oin)
#> Warning: Erroneous oin(s) (set to NA).
#> [1] "556000-4615" "232100-0156" "802002-4280" "802002-4280" NA           
#> Organizational identity number(s)
```
