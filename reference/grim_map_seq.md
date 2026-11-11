# GRIM-test values around the reported ones

`grim_map_seq()` varies the reported means and sample sizes a few steps
up and down and GRIM-tests each resulting value set. This shows whether
a small error in reporting or computing could explain an inconsistency.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
grim_map_seq(
  data,
  digits_x,
  x = NULL,
  n = NULL,
  ...,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE,
  var = c("x", "n"),
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
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  could take.

- digits_x:

  Integer. The number of decimal places in `x`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`). Either a single number for the whole column or one
  number per row of `data`. If left out, it is taken from a `digits_x`
  column of `data`. It also sets the step size for dispersing `x`.

- x, n:

  Optionally, the names of the columns in `data` that contain these
  values, if they differ from the defaults.

- ...:

  These dots must be empty. Every argument after them must be named in
  full.

- items_averaged:

  Integer. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md): the
  number of items averaged into each score, not the number of scale
  points. Give it as an argument or as a column of `data`; there is no
  default. It is returned as a column.

- data_type:

  String. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md):
  `"integer"` or `"continuous"`. Like `items_averaged`, give it as an
  argument or as a column of `data`; there is no default. It is returned
  as a column.

- x_meaning:

  String. What the `x` values are: `"mean"` (the default) or
  `"percentage"`. Percentages are divided by 100 and tested with two
  more decimal places. It applies to the whole call, so test a table
  that mixes means and percentages in parts, as in the examples of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
  The parts record different settings, so they can't be combined with
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  See *Defaults* in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- rounding, threshold, symmetric:

  Passed on to
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  whose section *Defaults* explains the defaults.

- show_rec:

  Logical. If `TRUE`, the reconstructed numbers are returned as columns;
  see *Reconstructed numbers* in
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
  Default is `FALSE`.

- var:

  String. The variables to disperse. Default is `c("x", "n")`.

- dispersion:

  Numeric. Steps up and down from the reported values, in units of the
  last decimal place. Default is `1:5`.

- out_min, out_max:

  Limits on the dispersed values. Both default to `"auto"`, which keeps
  `n` at 1 or above and leaves `x` unbounded. A number applies to every
  dispersed variable, and `NULL` removes the limit.

- include_reported:

  Logical. Should the reported values themselves be part of the
  sequences? Default is `FALSE`.

- include_consistent:

  Logical. Should consistent value sets be dispersed as well, not just
  inconsistent ones? Default is `FALSE`.

## Value

A tibble with the columns of
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
output plus `diff_var`, the number of steps between the tested value and
the reported one; `case`, the row number of the reported value set in
`data`; and `var`, the dispersed variable. Other columns of `data`, such
as study labels, play no role in the test and are dropped; use `case` to
join them back.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grim_map_seq()` output returns a tibble with one row per case and these
columns:

- `x` and `n`: the reported values, with `items_averaged`, `data_type`,
  and their `consistency`.

- `hits_total`: the number of GRIM-consistent value sets found within
  the `dispersion` range.

- `hits_x`, `hits_n`: the number found by varying `x` and `n`,
  respectively. If both `include_reported` and `include_consistent` are
  `TRUE`, a consistent reported value set is counted as well: once per
  variable here, and once in `hits_total`.

- `diff_x`: the distance from `x` to the nearest consistent dispersed
  value, in dispersion steps. `diff_x_up` and `diff_x_down` are the
  signed steps to the nearest consistent value above and below (`_down`
  is negative).

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
# `grim_map_seq()` takes the same input as `grim_map()`:
pigs1
#> # A tibble: 12 × 2
#>        x     n
#>    <dbl> <dbl>
#>  1  7.22    32
#>  2  4.74    25
#>  3  5.23    29
#>  4  2.57    24
#>  5  6.77    27
#>  6  2.68    28
#>  7  7.01    29
#>  8  7.38    26
#>  9  3.14    27
#> 10  6.89    31
#> 11  5       25
#> 12  0.24    28

out <- grim_map_seq(
  pigs1,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer",
  include_consistent = TRUE
)
out
#> # A tibble: 240 × 10
#> # GRIM: include_consistent = TRUE
#>        x     n items_averaged data_type digits_x consistency exp_incon diff_var
#>    <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl>    <int>
#>  1  7.17    32              1 integer          2 FALSE            0.64       -5
#>  2  7.18    32              1 integer          2 FALSE            0.64       -4
#>  3  7.19    32              1 integer          2 TRUE             0.64       -3
#>  4  7.2     32              1 integer          2 FALSE            0.64       -2
#>  5  7.21    32              1 integer          2 FALSE            0.64       -1
#>  6  7.23    32              1 integer          2 FALSE            0.64        1
#>  7  7.24    32              1 integer          2 FALSE            0.64        2
#>  8  7.25    32              1 integer          2 TRUE             0.64        3
#>  9  7.26    32              1 integer          2 FALSE            0.64        4
#> 10  7.27    32              1 integer          2 FALSE            0.64        5
#> # ℹ 230 more rows
#> # ℹ 2 more variables: case <int>, var <chr>

# Case-wise summaries:
out |>
  audit()
#> # A tibble: 12 × 14
#>        x     n items_averaged data_type consistency hits_total hits_x hits_n
#>    <dbl> <int>          <int> <chr>     <lgl>            <int>  <int>  <int>
#>  1  7.22    32              1 integer   TRUE                 5      2      3
#>  2  4.74    25              1 integer   FALSE                4      2      2
#>  3  5.23    29              1 integer   FALSE                6      3      3
#>  4  2.57    24              1 integer   FALSE                6      3      3
#>  5  6.77    27              1 integer   FALSE                7      3      4
#>  6  2.68    28              1 integer   TRUE                 4      2      2
#>  7  7.01    29              1 integer   FALSE                3      3      0
#>  8  7.38    26              1 integer   TRUE                 5      2      3
#>  9  3.14    27              1 integer   FALSE                6      3      3
#> 10  6.89    31              1 integer   FALSE                8      4      4
#> 11  5       25              1 integer   TRUE                12      2     10
#> 12  0.24    28              1 integer   FALSE                6      3      3
#> # ℹ 6 more variables: diff_x <int>, diff_x_up <int>, diff_x_down <int>,
#> #   diff_n <int>, diff_n_up <int>, diff_n_down <int>
```
