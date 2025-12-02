# Using DEBIT with dispersed inputs

`debit_map_seq()` applies DEBIT with values surrounding the input
values. This provides an easy and powerful way to assess whether small
errors in computing or reporting may be responsible for DEBIT
inconsistencies in published statistics.

## Usage

``` r
debit_map_seq(
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
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  could take.

- x, sd, n:

  Optionally, specify column names in `data` as these arguments.

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
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).

## Value

A tibble (data frame) with detailed test results.

## Summaries with [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

You can call
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
following `debit_map_seq()`. It will return a data frame with these
columns:

- `x`, `sd`, and `n` are the original inputs, tested for `consistency`
  here.

- `hits_total` is the total number of DEBIT-consistent value sets found
  within the specified `dispersion` range.

- `hits_x` is the number of DEBIT-consistent value sets found by varying
  `x`.

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
# `debit_map_seq()` can take any input
# that `debit_map()` can take:
pigs3
#> # A tibble: 7 × 3
#>   x     sd        n
#>   <chr> <chr> <dbl>
#> 1 0.53  0.50   1683
#> 2 0.44  0.50   1683
#> 3 0.77  0.42   1683
#> 4 0.19  0.35   1683
#> 5 0.34  0.47   1683
#> 6 0.93  0.25   1683
#> 7 0.12  0.33   1683

# Results from testing some few rows:
out <- pigs3 %>%
  dplyr::slice(3:4) %>%
  debit_map_seq()

out
#> # A tibble: 30 × 14
#>    x     sd        n consistency rounding   sd_lower sd_incl_lower sd_upper
#>    <chr> <chr> <int> <lgl>       <chr>         <dbl> <lgl>            <dbl>
#>  1 0.14  0.35   1683 TRUE        up_or_down    0.345 TRUE             0.355
#>  2 0.15  0.35   1683 TRUE        up_or_down    0.345 TRUE             0.355
#>  3 0.16  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  4 0.17  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  5 0.18  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  6 0.20  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  7 0.21  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  8 0.22  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#>  9 0.23  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#> 10 0.24  0.35   1683 FALSE       up_or_down    0.345 TRUE             0.355
#> # ℹ 20 more rows
#> # ℹ 6 more variables: sd_incl_upper <lgl>, x_lower <dbl>, x_upper <dbl>,
#> #   diff_var <int>, case <int>, var <chr>

# Case-wise summaries with `audit_seq()`
# can be more important than the raw results:
out %>%
  audit_seq()
#> # A tibble: 1 × 17
#>   x     sd        n consistency hits_total hits_x hits_sd hits_n diff_x
#>   <chr> <chr> <int> <lgl>            <int>  <int>   <int>  <int>  <int>
#> 1 0.19  0.35   1683 FALSE                4      2       2      0      4
#> # ℹ 8 more variables: diff_x_up <int>, diff_x_down <int>, diff_sd <int>,
#> #   diff_sd_up <int>, diff_sd_down <int>, diff_n <int>, diff_n_up <int>,
#> #   diff_n_down <int>
```
