# Create S3 methods

Helper function to create S3 method that preserves (most)

## Usage

``` r
create_s3_method(generic = NULL, object = NULL)
```

## Arguments

- generic, object:

  arguments passed to
  [`NextMethod`](https://rdrr.io/r/base/UseMethod.html)

## Value

Function with arguments "x", "i", and "...". The "i" argument can be
thought of as "index" if the method defined is for example the
"\["-function.
