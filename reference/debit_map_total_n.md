# DEBIT-test with hypothetical group sizes

Some studies report group means and standard deviations of binary data
but only the total sample size, not the group sizes. For two groups,
`debit_map_total_n()` tests hypothetical splits of the total, as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
does.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
debit_map_total_n(
  data,
  digits_x,
  digits_sd,
  x1 = NULL,
  x2 = NULL,
  sd1 = NULL,
  sd2 = NULL,
  n = NULL,
  ...,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE,
  dispersion = 0:5,
  n_min = NULL,
  n_max = NULL,
  constant = NULL
)
```

## Arguments

- data:

  Data frame with numeric columns `x1`, `x2`, `sd1`, `sd2`, and `n`: the
  two group means, the two group SDs, and the total sample size. Which
  group is 1 does not matter, since both pairings with the group sizes
  are tested, but `x1` must belong with `sd1` and `x2` with `sd2`.

- digits_x, digits_sd:

  Integer. The number of decimal places in the means and SDs, including
  trailing zeros. There are no defaults, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for both groups,
  or two numbers, one per group, which apply to every row of `data`. If
  left out, they are taken from `digits_x` and `digits_sd` columns of
  `data`, with one number per row for both groups. If the decimal places
  differ both by group and by row, call the function on each set of rows
  separately and
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  each output.

- x1, x2, sd1, sd2, n:

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

- dispersion:

  Numeric. Steps up and down from half the total `n`. Default is `0:5`,
  i.e., the even split and five steps away from it.

- n_min:

  Numeric. Minimal group size. Default is `NULL`: 2 under
  `sd_meaning = "sd_sample"` or `"se"`, which divide by `n - 1`, and 1
  under `"sd_pop"` or `"sd_sample_or_pop"`.

- n_max:

  Numeric. Maximal group size. Default is `NULL`, i.e., no maximum.

- constant:

  Optionally, values that belong to the two groups, such as their
  labels: a length-2 vector for the groups of `x1` and `x2`, or a list
  of such vectors. Each becomes a column, as in
  [`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
  and its values stay with their groups in both directions. Default is
  `NULL`.

## Value

A tibble like that of
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md),
with `x` and `sd` columns for the reported values, `dir` recording which
group is paired with the larger group size (the equal split of an even
total appears under `"forth"` only), the other columns of
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
output, and the other columns of `data`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`debit_map_total_n()` output returns a tibble with one row per case and
these columns:

- `x1`, `x2`, `sd1`, `sd2`, and `n`: the reported values.

- `hits_total`: the number of scenarios in which both groups are
  DEBIT-consistent, summing `hits_forth` and `hits_back`.

- `hits_forth`: the number of such scenarios with `x2` and `sd2` paired
  with the larger group.

- `hits_back`: the same with `x1` and `sd1` paired with the larger
  group.

- `scenarios_total`: the number of scenarios tested, each a split of `n`
  with one pairing of groups and statistics, leaving out those that
  could not be decided.

- `hit_rate`: `hits_total / scenarios_total`, or `NA` if no scenario
  could be decided.

A count is `NA` if it depends on a scenario that could not be decided,
or on rows removed from the output, as by
[`filter()`](https://rdrr.io/r/stats/filter.html).

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
again on that summary to summarize these columns further.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which created this function from
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).

## Examples

``` r
df <- tibble::tribble(
  ~x1,  ~x2,  ~sd1,  ~sd2,  ~n,
  0.30, 0.28, 0.17, 0.10, 70,
  0.41, 0.39, 0.09, 0.15, 65
)

debit_map_total_n(df, digits_x = 2, digits_sd = 2, data_type = "integer")
#> # A tibble: 46 × 11
#> # DEBIT: default settings
#>        x    sd     n n_change data_type digits_x digits_sd consistency
#>    <dbl> <dbl> <int>    <int> <chr>        <int>     <int> <lgl>      
#>  1  0.3   0.17    35        0 integer          2         2 FALSE      
#>  2  0.28  0.1     35        0 integer          2         2 FALSE      
#>  3  0.3   0.17    34       -1 integer          2         2 FALSE      
#>  4  0.28  0.1     36        1 integer          2         2 FALSE      
#>  5  0.3   0.17    33       -2 integer          2         2 FALSE      
#>  6  0.28  0.1     37        2 integer          2         2 FALSE      
#>  7  0.3   0.17    32       -3 integer          2         2 FALSE      
#>  8  0.28  0.1     38        3 integer          2         2 FALSE      
#>  9  0.3   0.17    31       -4 integer          2         2 FALSE      
#> 10  0.28  0.1     39        4 integer          2         2 FALSE      
#> # ℹ 36 more rows
#> # ℹ 3 more variables: both_consistent <lgl>, case <int>, dir <fct>

df |>
  debit_map_total_n(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  audit()
#> # A tibble: 2 × 10
#>      x1    x2   sd1   sd2     n hits_total hits_forth hits_back scenarios_total
#>   <dbl> <dbl> <dbl> <dbl> <int>      <int>      <int>     <int>           <int>
#> 1  0.3   0.28  0.17  0.1     70          0          0         0              11
#> 2  0.41  0.39  0.09  0.15    65          0          0         0              12
#> # ℹ 1 more variable: hit_rate <dbl>
```
