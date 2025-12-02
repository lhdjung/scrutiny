# GRIMMER-testing with dispersed inputs

`grimmer_map_seq()` performs GRIMMER-testing with values surrounding the
input values. This provides an easy and powerful way to assess whether
small errors in computing or reporting may be responsible for GRIMMER
inconsistencies in published statistics.

Call
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
on the results for summary statistics.

## Usage

``` r
grimmer_map_seq(
  data,
  x = NULL,
  sd = NULL,
  n = NULL,
  var = Inf,
  dispersion = 1:5,
  out_min = "auto",
  out_max = NULL,
  include_reported = FALSE,
  include_consistent = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame that
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
  could take.

- x, sd, n:

  Optionally, specify these arguments as column names in `data`.

- var:

  String. Names of the columns that will be dispersed. Default is
  `c("x", "sd", "n")`.

- dispersion:

  Numeric. Sequence with steps up and down from the `var` inputs. It
  will be adjusted to these values' decimal levels. For example, with a
  reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
  steps up and down.

- out_min, out_max:

  If specified, output will be restricted so that it's not below
  `out_min` or above `out_max`. Defaults are `"auto"` for `out_min`,
  i.e., a minimum of one decimal unit above zero; and `NULL` for
  `out_max`, i.e., no maximum.

- include_reported:

  Logical. Should the reported values themselves be included in the
  sequences originating from them? Default is `FALSE` because this might
  be redundant and bias the results.

- include_consistent:

  Logical. Should the function also process consistent cases (from among
  those reported), not just inconsistent ones? Default is `FALSE`
  because the focus should be on clarifying inconsistencies.

- ...:

  Arguments passed down to
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md).

## Value

A tibble (data frame) with detailed test results. See
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
for an explanation of the `reason` column.

## Summaries with [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

You can call
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
following `grimmer_map_seq()`. It will return a data frame with these
columns:

- `x`, `sd`, and `n` are the original inputs, tested for `consistency`
  here.

- `hits_total` is the total number of GRIMMER-consistent value sets
  found within the specified `dispersion` range.

- `hits_x` is the number of GRIMMER-consistent value sets found by
  varying `x`.

- Accordingly with `sd` and `hits_sd` as well as `n` and `hits_n`.

- (Note that any consistent reported cases will be counted by the
  `hits_*` columns if both `include_reported` and `include_consistent`
  are set to `TRUE`.)

- `diff_x` reports the absolute difference between `x` and the next
  consistent dispersed value (in dispersion steps, not the actual
  numeric difference). `diff_x_up` and `diff_x_down` report the
  difference to the next higher or lower consistent value, respectively.

- `diff_sd`, `diff_sd_up`, and `diff_sd_down` do the same for `sd`.

- Likewise with `diff_n`, `diff_n_up`, and `diff_n_down`.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
to summarize results even further. It's mostly self-explaining, but
`na_count` and `na_rate` are the number and rate of times that a
difference could not be computed because of a lack of corresponding hits
within the `dispersion` range.

## Examples

``` r
# `grimmer_map_seq()` can take any input
# that `grimmer_map()` can take:
pigs5
#> # A tibble: 12 × 3
#>    x     sd        n
#>    <chr> <chr> <dbl>
#>  1 7.22  5.30     38
#>  2 4.74  6.55     31
#>  3 5.23  2.55     35
#>  4 2.57  2.57     30
#>  5 6.77  2.18     33
#>  6 2.68  2.59     34
#>  7 7.01  6.68     35
#>  8 7.38  3.65     32
#>  9 3.14  5.32     33
#> 10 6.89  4.18     37
#> 11 5.00  2.18     31
#> 12 0.24  6.43     34

# All the results:
out <- pigs5 %>%
  dplyr::slice(1:3) %>%
  grimmer_map_seq(include_consistent = TRUE)
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>

out
#> # A tibble: 90 × 8
#>    x     sd        n consistency reason            diff_var  case var  
#>    <chr> <chr> <int> <lgl>       <chr>                <int> <int> <chr>
#>  1 7.17  5.30     38 FALSE       GRIM inconsistent       -5     1 x    
#>  2 7.18  5.30     38 TRUE        Passed all              -4     1 x    
#>  3 7.19  5.30     38 FALSE       GRIM inconsistent       -3     1 x    
#>  4 7.20  5.30     38 FALSE       GRIM inconsistent       -2     1 x    
#>  5 7.21  5.30     38 TRUE        Passed all              -1     1 x    
#>  6 7.23  5.30     38 FALSE       GRIM inconsistent        1     1 x    
#>  7 7.24  5.30     38 TRUE        Passed all               2     1 x    
#>  8 7.25  5.30     38 FALSE       GRIM inconsistent        3     1 x    
#>  9 7.26  5.30     38 TRUE        Passed all               4     1 x    
#> 10 7.27  5.30     38 FALSE       GRIM inconsistent        5     1 x    
#> # ℹ 80 more rows

# Case-wise summaries with `audit_seq()`
# can be more important than the raw results:
out %>%
  audit_seq()
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> # A tibble: 3 × 17
#>   x     sd        n consistency hits_total hits_x hits_sd hits_n diff_x
#>   <chr> <chr> <int> <lgl>            <int>  <int>   <int>  <int>  <int>
#> 1 7.22  5.30     38 FALSE                8      4       0      4      1
#> 2 4.74  6.55     31 TRUE                15      2      10      3      3
#> 3 5.23  2.55     35 FALSE               15      2      10      3      3
#> # ℹ 8 more variables: diff_x_up <int>, diff_x_down <int>, diff_sd <int>,
#> #   diff_sd_up <int>, diff_sd_down <int>, diff_n <int>, diff_n_up <int>,
#> #   diff_n_down <int>
```
