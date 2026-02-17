# Formatting pin

Format `pin` for pretty printing

## Usage

``` r
format_pin(x, format. = "%Y%m%d%N", ...)
```

## Arguments

- x:

  vector of class "pin" (see
  [`as.pin`](https://ropengov.github.io/sweidnumbr/reference/as.pin.md))
  or a vector that can be coerced to such

- format.:

  character string specifying the output format. `%N` is used as a
  reference for the last four digits of the pin. Format of the date is
  handled via [`strptime`](https://rdrr.io/r/base/strptime.html).
  (`"%Y%m%d%N"` by default). `%P` is an available shorthand for
  `"(%C) %y-%m-%d - %N"`, a format aimed for maximal readability when
  used in long lists

- ...:

  arguments passed to
  [`format.Date`](https://rdrr.io/r/base/as.Date.html)

## Value

character vector of same length as `x`

## Examples

``` r
x <- as.pin(fake_pins$pin[1:10])
#> Assumption: Pin of format YYMMDDNNNC is assumed to be less than 100 years old

# Separate elements with hyphens:
format_pin(x, "%Y-%m-%d-%N")
#>  [1] "1912-12-12-1212" "2012-12-12-1212" "1912-12-12-1212" "2012-12-12-1212"
#>  [5] "1991-06-25-2523" "1896-11-07-0798" "1900-11-12-1298" "2008-09-05-0523"
#>  [9] "1891-01-25-2598" "2014-01-23-2323"


# Separate even further
format_pin(x, "%C-%y-%m-%d-%N")
#>  [1] "19-12-12-12-1212" "20-12-12-12-1212" "19-12-12-12-1212" "20-12-12-12-1212"
#>  [5] "19-91-06-25-2523" "18-96-11-07-0798" "19-00-11-12-1298" "20-08-09-05-0523"
#>  [9] "18-91-01-25-2598" "20-14-01-23-2323"

# The special P-format for maximal readability
format_pin(x, "%P") 
#>  [1] "(19) 12-12-12 - 1212" "(20) 12-12-12 - 1212" "(19) 12-12-12 - 1212"
#>  [4] "(20) 12-12-12 - 1212" "(19) 91-06-25 - 2523" "(18) 96-11-07 - 0798"
#>  [7] "(19) 00-11-12 - 1298" "(20) 08-09-05 - 0523" "(18) 91-01-25 - 2598"
#> [10] "(20) 14-01-23 - 2323"

# A custom representation
format_pin(x, "Borned %d of %B in %Y (a %A in week %U) with suffix no: %N")
#>  [1] "Borned 12 of December in 1912 (a Thursday in week 49) with suffix no: 1212" 
#>  [2] "Borned 12 of December in 2012 (a Wednesday in week 50) with suffix no: 1212"
#>  [3] "Borned 12 of December in 1912 (a Thursday in week 49) with suffix no: 1212" 
#>  [4] "Borned 12 of December in 2012 (a Wednesday in week 50) with suffix no: 1212"
#>  [5] "Borned 25 of June in 1991 (a Tuesday in week 25) with suffix no: 2523"      
#>  [6] "Borned 07 of November in 1896 (a Saturday in week 44) with suffix no: 0798" 
#>  [7] "Borned 12 of November in 1900 (a Monday in week 45) with suffix no: 1298"   
#>  [8] "Borned 05 of September in 2008 (a Friday in week 35) with suffix no: 0523"  
#>  [9] "Borned 25 of January in 1891 (a Sunday in week 04) with suffix no: 2598"    
#> [10] "Borned 23 of January in 2014 (a Thursday in week 03) with suffix no: 2323"  

# Extract only the year
format_pin(x, "%Y")
#>  [1] "1912" "2012" "1912" "2012" "1991" "1896" "1900" "2008" "1891" "2014"
```
