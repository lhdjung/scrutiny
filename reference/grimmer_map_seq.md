# GRIMMER-test values around the reported ones

`grimmer_map_seq()` varies the reported means, standard deviations, and
sample sizes a few steps up and down and GRIMMER-tests each resulting
value set. This shows whether a small error in reporting or computing
could explain an inconsistency.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
grimmer_map_seq(
  data,
  digits_x,
  digits_sd,
  x = NULL,
  sd = NULL,
  n = NULL,
  ...,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
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
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
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

- items_averaged:

  Integer. As in
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md):
  the number of items averaged into each score, given as an argument or
  as a column of `data`. There is no default.

- data_type:

  String. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md):
  `"integer"` or `"continuous"`. Like `items_averaged`, give it as an
  argument or as a column of `data`; there is no default. It is returned
  as a column.

- rounding, threshold, symmetric, sd_meaning:

  Passed on to
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  whose section *Defaults* explains the defaults.

- var:

  String. The variables to disperse. Default is `c("x", "sd", "n")`.

- dispersion:

  Numeric. Steps up and down from the reported values, in units of the
  last decimal place. Default is `1:5`.

- out_min, out_max:

  Limits on the dispersed values. Both default to `"auto"`, which keeps
  `n` at 2 or above (1 under `sd_meaning = "sd_pop"` or
  `"sd_sample_or_pop"`) and `sd` at 0 or above, and leaves `x`
  unbounded. A number applies to every dispersed variable, and `NULL`
  removes the limit.

- include_reported:

  Logical. Should the reported values themselves be part of the
  sequences? Default is `FALSE`.

- include_consistent:

  Logical. Should consistent value sets be dispersed as well, not just
  inconsistent ones? Default is `FALSE`.

## Value

A tibble with the columns of
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
output plus `diff_var`, `case`, and `var`, as in
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
As there, the other columns of `data` are dropped.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grimmer_map_seq()` output returns a tibble with one row per case and
these columns:

- `x`, `sd`, and `n`: the reported values, with `items_averaged`,
  `data_type`, and their `consistency`.

- `hits_total`: the number of GRIMMER-consistent value sets found within
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
# `grimmer_map_seq()` takes the same input as `grimmer_map()`:
pigs5
#> # A tibble: 12 × 3
#>        x    sd     n
#>    <dbl> <dbl> <dbl>
#>  1  7.22  5.3     38
#>  2  4.74  6.55    31
#>  3  5.23  2.55    35
#>  4  2.57  2.57    30
#>  5  6.77  2.18    33
#>  6  2.68  2.59    34
#>  7  7.01  6.68    35
#>  8  7.38  3.65    32
#>  9  3.14  5.32    33
#> 10  6.89  4.18    37
#> 11  5     2.18    31
#> 12  0.24  6.43    34

out <- grimmer_map_seq(
  pigs5,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer",
  include_consistent = TRUE
)
out
#> # A tibble: 360 × 12
#> # GRIMMER: include_consistent = TRUE
#>        x    sd     n items_averaged data_type digits_x digits_sd consistency
#>    <dbl> <dbl> <int>          <int> <chr>        <int>     <int> <lgl>      
#>  1  7.17   5.3    38              1 integer          2         2 FALSE      
#>  2  7.18   5.3    38              1 integer          2         2 TRUE       
#>  3  7.19   5.3    38              1 integer          2         2 FALSE      
#>  4  7.2    5.3    38              1 integer          2         2 FALSE      
#>  5  7.21   5.3    38              1 integer          2         2 TRUE       
#>  6  7.23   5.3    38              1 integer          2         2 FALSE      
#>  7  7.24   5.3    38              1 integer          2         2 TRUE       
#>  8  7.25   5.3    38              1 integer          2         2 FALSE      
#>  9  7.26   5.3    38              1 integer          2         2 TRUE       
#> 10  7.27   5.3    38              1 integer          2         2 FALSE      
#> # ℹ 350 more rows
#> # ℹ 4 more variables: reason <chr>, diff_var <int>, case <int>, var <chr>

# Case-wise summaries:
out |>
  audit()
#> # A tibble: 12 × 19
#>        x    sd     n items_averaged data_type consistency hits_total hits_x
#>    <dbl> <dbl> <int>          <int> <chr>     <lgl>            <int>  <int>
#>  1  7.22  5.3     38              1 integer   FALSE                8      4
#>  2  4.74  6.55    31              1 integer   TRUE                15      2
#>  3  5.23  2.55    35              1 integer   FALSE               15      2
#>  4  2.57  2.57    30              1 integer   TRUE                11      2
#>  5  6.77  2.18    33              1 integer   FALSE                5      3
#>  6  2.68  2.59    34              1 integer   TRUE                13      2
#>  7  7.01  6.68    35              1 integer   FALSE                4      4
#>  8  7.38  3.65    32              1 integer   TRUE                16      3
#>  9  3.14  5.32    33              1 integer   FALSE                9      4
#> 10  6.89  4.18    37              1 integer   TRUE                16      3
#> 11  5     2.18    31              1 integer   TRUE                15      2
#> 12  0.24  6.43    34              1 integer   TRUE                17      3
#> # ℹ 11 more variables: hits_sd <int>, hits_n <int>, diff_x <int>,
#> #   diff_x_up <int>, diff_x_down <int>, diff_sd <int>, diff_sd_up <int>,
#> #   diff_sd_down <int>, diff_n <int>, diff_n_up <int>, diff_n_down <int>
```
