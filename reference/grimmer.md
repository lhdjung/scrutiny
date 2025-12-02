# The GRIMMER test (granularity-related inconsistency of means mapped to error repeats)

`grimmer()` checks if reported mean and SD values of integer data are
mathematically consistent with the reported sample size and the number
of items that compose the mean value. It works much like
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

The function is vectorized, but it is recommended to use
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
for testing multiple cases.

## Usage

``` r
grimmer(
  x,
  sd,
  n,
  items = 1,
  show_reason = FALSE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
)
```

## Arguments

- x:

  String. The reported mean value.

- sd:

  String. The reported standard deviation.

- n:

  Integer. The reported sample size.

- items:

  Integer. The number of items composing the `x` and `sd` values.
  Default is `1`, the most common case.

- show_reason:

  Logical. For internal use only. If set to `TRUE`, the output is a list
  of length-2 lists which also contain the reasons for inconsistencies.
  Don't specify this manually; instead, use `show_reason` in
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md).
  See there for explanation. Default is `FALSE`.

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

Logical. `TRUE` if `x`, `sd`, `n`, and `items` are mutually consistent,
`FALSE` if not.

## Details

GRIMMER was originally devised by Anaya (2016). The present
implementation follows Allard's (2018) refined Analytic-GRIMMER
algorithm. It uses a variant of Analytic-GRIMMER first implemented in
[`rsprite2::GRIMMER_test()`](https://lukaswallrich.github.io/rsprite2/reference/GRIMMER_test.html)
that can be applied to multi-item scales.

The scrutiny version embeds GRIMMER in the broader system of consistency
testing, as laid out in [*Consistency tests in
depth*](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.html).
The `grimmer()` function is a vectorized (multiple-case) version of this
basic implementation. For more context and variable name translations,
see the top of the R/grimmer.R source file.

## References

Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
possibility of standard deviations.
https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/

Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
reported measures of variability. *PeerJ Preprints.*
https://peerj.com/preprints/2400v1/

## Examples

``` r
# A mean of 5.23 is not consistent with an SD of 2.55
# and a sample size of 35:
grimmer(x = "5.23", sd = "2.55", n = 35)
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#>  5.23 
#> FALSE 

# However, mean and SD are consistent with a
# sample size of 31:
grimmer(x = "5.23", sd = "2.55", n = 31)
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> 5.23 
#> TRUE 

# For a scale composed of two items:
grimmer(x = "2.74", sd = "0.96", n = 63, items = 2)
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> 2.74 
#> TRUE 
```
