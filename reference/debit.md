# The DEBIT (descriptive binary) test

`debit()` tests summaries of binary data for consistency: If the mean
and the sample standard deviation of binary data are given, are they
consistent with the reported sample size?

The function is vectorized, but it is recommended to use
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
for testing multiple cases.

## Usage

``` r
debit(
  x,
  sd,
  n,
  formula = "mean_n",
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  String. Mean of a binary distribution.

- sd:

  String. Sample standard deviation of a binary distribution.

- n:

  Integer. Total sample size.

- formula:

  String. Formula used to compute the SD of the binary distribution.
  Currently, only the default, `"mean_n"`, is supported.

- rounding:

  String. Rounding method or methods to be used for reconstructing the
  SD values to which `sd` will be compared. Default is `"up_or_down"`
  (from 5). See
  [`vignette("rounding-options")`](https://lhdjung.github.io/scrutiny/articles/rounding-options.md).

- threshold:

  Integer. If `rounding` is set to `"up_from"`, `"down_from"`, or
  `"up_from_or_down_from"`, set `threshold` to the number from which the
  reconstructed values should then be rounded up or down. Otherwise
  irrelevant. Default is `5`.

- symmetric:

  Logical. Set `symmetric` to `TRUE` if the rounding of negative numbers
  with `"up"`, `"down"`, `"up_from"`, or `"down_from"` should mirror
  that of positive numbers so that their absolute values are always
  equal. Default is `FALSE`.

## Value

Logical. `TRUE` if `x`, `sd`, and `n` are mutually consistent, `FALSE`
if not.

## References

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## See also

[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
applies `debit()` to any number of cases at once.

## Examples

``` r
# Check single cases of binary
# summary data:
debit(x = "0.36", sd = "0.11", n = 20)
#>  0.36 
#> FALSE 
```
