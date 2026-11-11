# DEBIT-test values around the reported ones

`debit_map_seq()` varies the reported means, standard deviations, and
sample sizes a few steps up and down and DEBIT-tests each resulting
value set. This shows whether a small error in reporting or computing
could explain an inconsistency.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
debit_map_seq(
  data,
  digits_x,
  digits_sd,
  x = NULL,
  sd = NULL,
  n = NULL,
  ...,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE,
  var = c("x", "sd", "n"),
  dispersion = 1:5,
  out_min = "auto",
  out_max = "auto",
  include_reported = FALSE,
  include_consistent = FALSE
)
```

## Arguments

- data:

  A data frame that
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  could take.

- digits_x, digits_sd:

  Integer. The number of decimal places in `x` and `sd`, including
  trailing zeros. There are no defaults, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for the whole
  column or one number per row of `data`. If left out, they are taken
  from `digits_x` and `digits_sd` columns of `data`. They also set the
  step sizes for dispersing `x` and `sd`.

- x, sd, n:

  Optionally, the names of the columns in `data` that contain these
  values, if they differ from the defaults.

- ...:

  These dots must be empty. Every argument after them must be named in
  full.

- data_type:

  String. As in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).
  Give it as an argument or as a column of `data`; there is no default.
  It is returned as a column.

- formula, rounding, threshold, symmetric, sd_meaning:

  Passed on to
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  which explains the defaults.

- show_rec:

  Logical. If `TRUE`, the reconstructed values are returned as further
  columns; see *Value* in
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).
  Default is `FALSE`.

- var:

  String. The variables to disperse. Default is `c("x", "sd", "n")`.

- dispersion:

  Numeric. Steps up and down from the reported values, in units of the
  last decimal place. Default is `1:5`.

- out_min, out_max:

  Limits on the dispersed values. Both default to `"auto"`, which keeps
  `n` at 2 or above (1 under `sd_meaning = "sd_pop"` or
  `"sd_sample_or_pop"`) and `x` and `sd` between 0 and 1. A number
  applies to every dispersed variable, and `NULL` removes the limit.

- include_reported:

  Logical. Should the reported values themselves be part of the
  sequences? Default is `FALSE`.

- include_consistent:

  Logical. Should consistent value sets be dispersed as well, not just
  inconsistent ones? Default is `FALSE`.

## Value

A tibble with the columns of
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
output plus `diff_var`, `case`, and `var`, as in
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
As there, the other columns of `data` are dropped.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`debit_map_seq()` output returns a tibble with one row per case and
these columns:

- `x`, `sd`, and `n`: the reported values, with `data_type` and their
  `consistency`.

- `hits_total`: the number of DEBIT-consistent value sets found within
  the `dispersion` range.

- `hits_x`, `hits_sd`, `hits_n`: the number found by varying `x`, `sd`,
  and `n`, respectively. If both `include_reported` and
  `include_consistent` are `TRUE`, a consistent reported value set is
  counted as well: once per variable here, and once in `hits_total`.

- `diff_x`: the distance from `x` to the nearest consistent dispersed
  value, in dispersion steps. `diff_x_up` and `diff_x_down` are the
  signed steps to the nearest consistent value above and below (`_down`
  is negative).

- `diff_sd`, `diff_sd_up`, `diff_sd_down`: the same for `sd`.

- `diff_n`, `diff_n_up`, `diff_n_down`: the same for `n`.

A count or distance is `NA` if it depends on a dispersed value set whose
consistency is `NA`, on a variable whose reported value is missing,
which is not dispersed, or on rows removed from the output, as by
[`filter()`](https://rdrr.io/r/stats/filter.html).

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
again on that summary to summarize these columns further. `na_count` and
`na_rate` there count the cases without a consistent value within the
`dispersion` range, and those that a missing value leaves unknown.

## Examples

``` r
# `debit_map_seq()` takes the same input as `debit_map()`:
pigs3
#> # A tibble: 7 × 3
#>       x    sd     n
#>   <dbl> <dbl> <dbl>
#> 1  0.53  0.5   1683
#> 2  0.44  0.5   1683
#> 3  0.77  0.42  1683
#> 4  0.19  0.35  1683
#> 5  0.34  0.47  1683
#> 6  0.93  0.25  1683
#> 7  0.12  0.33  1683

out <- pigs3 |>
  dplyr::slice(3:4) |>
  debit_map_seq(digits_x = 2, digits_sd = 2, data_type = "integer")
out
#> # A tibble: 30 × 10
#> # DEBIT: default settings
#>        x    sd     n data_type digits_x digits_sd consistency diff_var  case
#>    <dbl> <dbl> <int> <chr>        <int>     <int> <lgl>          <int> <int>
#>  1  0.14  0.35  1683 integer          2         2 TRUE              -5     2
#>  2  0.15  0.35  1683 integer          2         2 TRUE              -4     2
#>  3  0.16  0.35  1683 integer          2         2 FALSE             -3     2
#>  4  0.17  0.35  1683 integer          2         2 FALSE             -2     2
#>  5  0.18  0.35  1683 integer          2         2 FALSE             -1     2
#>  6  0.2   0.35  1683 integer          2         2 FALSE              1     2
#>  7  0.21  0.35  1683 integer          2         2 FALSE              2     2
#>  8  0.22  0.35  1683 integer          2         2 FALSE              3     2
#>  9  0.23  0.35  1683 integer          2         2 FALSE              4     2
#> 10  0.24  0.35  1683 integer          2         2 FALSE              5     2
#> # ℹ 20 more rows
#> # ℹ 1 more variable: var <chr>

# Case-wise summaries:
out |>
  audit()
#> # A tibble: 1 × 18
#>       x    sd     n data_type consistency hits_total hits_x hits_sd hits_n
#>   <dbl> <dbl> <int> <chr>     <lgl>            <int>  <int>   <int>  <int>
#> 1  0.19  0.35  1683 integer   FALSE                4      2       2      0
#> # ℹ 9 more variables: diff_x <int>, diff_x_up <int>, diff_x_down <int>,
#> #   diff_sd <int>, diff_sd_up <int>, diff_sd_down <int>, diff_n <int>,
#> #   diff_n_up <int>, diff_n_down <int>
```
