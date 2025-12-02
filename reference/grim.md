# The GRIM test (granularity-related inconsistency of means)

`grim()` checks if a reported mean value of integer data is
mathematically consistent with the reported sample size and the number
of items that compose the mean value.

Set `percent` to `TRUE` if `x` is a percentage. This will convert `x` to
a decimal number and adjust the decimal count accordingly.

The function is vectorized, but it is recommended to use
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
for testing multiple cases.

## Usage

``` r
grim(
  x,
  n,
  items = 1,
  percent = FALSE,
  show_rec = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
)
```

## Arguments

- x:

  String. The reported mean or percentage value.

- n:

  Integer. The reported sample size.

- items:

  Numeric. The number of items composing `x`. Default is 1, the most
  common case.

- percent:

  Logical. Set `percent` to `TRUE` if `x` is a percentage. This will
  convert it to a decimal number and adjust the decimal count (i.e.,
  increase it by 2). Default is `FALSE`.

- show_rec:

  Logical. For internal use only. If set to `TRUE`, the output is a
  matrix that also contains intermediary values from GRIM-testing. Don't
  specify this manually; instead, use `show_rec` in
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
  Default is `FALSE`.

- rounding:

  String. Rounding method or methods to be used for reconstructing the
  values to which `x` will be compared. Default is `"up_or_down"` (from
  5).

- threshold:

  Numeric. If `rounding` is set to `"up_from"`, `"down_from"`, or
  `"up_from_or_down_from"`, set `threshold` to the number from which the
  reconstructed values should then be rounded up or down. Otherwise,
  this argument plays no role. Default is `5`.

- symmetric:

  Logical. Set `symmetric` to `TRUE` if the rounding of negative numbers
  with `"up"`, `"down"`, `"up_from"`, or `"down_from"` should mirror
  that of positive numbers so that their absolute values are always
  equal. Default is `FALSE`.

- tolerance:

  Numeric. Tolerance of comparison between `x` and the possible mean or
  percentage values. Default is circa 0.000000015 (1.490116e-08), as in
  [`dplyr::near()`](https://dplyr.tidyverse.org/reference/near.html).

## Value

Logical. `TRUE` if `x`, `n`, and `items` are mutually consistent,
`FALSE` if not.

## Details

The `x` values need to be strings because only strings retain trailing
zeros, which are as important for the GRIM test as any other decimal
digits.

Use
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
on numeric values (or values that were numeric values at some point) to
easily supply the trailing zeros they might once have had. See
documentation there.

Browse the source code in the grim.R file. `grim()` is a vectorized
version of the internal `grim_scalar()` function found there.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
applies `grim()` to any number of cases at once.

## Examples

``` r
# A mean of 5.19 is not consistent with a sample size of 28:
grim(x = "5.19", n = 28)    # `x` in quotes!
#>  5.19 
#> FALSE 

# However, it is consistent with a sample size of 32:
grim(x = "5.19", n = 32)
#> 5.19 
#> TRUE 

# For a scale composed of two items:
grim(x = "2.84", n = 16, items = 2)
#> 2.84 
#> TRUE 

# With percentages instead of means -- here, 71%:
grim(x = "71", n = 43, percent = TRUE)
#>    71 
#> FALSE 
```
