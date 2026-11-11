# GRIMMER-test with hypothetical group sizes

Some studies report group means and standard deviations but only the
total sample size, not the group sizes. For two groups,
`grimmer_map_total_n()` tests hypothetical splits of the total, as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
does.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
grimmer_map_total_n(
  data,
  digits_x,
  digits_sd,
  x1 = NULL,
  x2 = NULL,
  sd1 = NULL,
  sd2 = NULL,
  n = NULL,
  ...,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
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
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
output, and the other columns of `data`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grimmer_map_total_n()` output returns a tibble with one row per case
and these columns:

- `x1`, `x2`, `sd1`, `sd2`, and `n`: the reported values.

- `hits_total`: the number of scenarios in which both groups are
  GRIMMER-consistent, summing `hits_forth` and `hits_back`.

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

Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
possibility of standard deviations.
https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which created this function from
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md).

## Examples

``` r
df <- tibble::tribble(
  ~x1,  ~x2,  ~sd1, ~sd2, ~n,
  3.43, 5.28, 1.09, 2.12, 70,
  2.97, 4.42, 0.43, 1.65, 65
)

grimmer_map_total_n(
  df,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 46 × 13
#> # GRIMMER: default settings
#>        x    sd     n n_change items_averaged data_type digits_x digits_sd
#>    <dbl> <dbl> <int>    <int>          <int> <chr>        <int>     <int>
#>  1  3.43  1.09    35        0              1 integer          2         2
#>  2  5.28  2.12    35        0              1 integer          2         2
#>  3  3.43  1.09    34       -1              1 integer          2         2
#>  4  5.28  2.12    36        1              1 integer          2         2
#>  5  3.43  1.09    33       -2              1 integer          2         2
#>  6  5.28  2.12    37        2              1 integer          2         2
#>  7  3.43  1.09    32       -3              1 integer          2         2
#>  8  5.28  2.12    38        3              1 integer          2         2
#>  9  3.43  1.09    31       -4              1 integer          2         2
#> 10  5.28  2.12    39        4              1 integer          2         2
#> # ℹ 36 more rows
#> # ℹ 5 more variables: consistency <lgl>, both_consistent <lgl>, reason <chr>,
#> #   case <int>, dir <fct>

# `hits_total` counts the splits in which both groups are consistent:
df |>
  grimmer_map_total_n(
    digits_x = 2,
    digits_sd = 2,
    items_averaged = 1,
    data_type = "integer"
  ) |>
  audit()
#> # A tibble: 2 × 10
#>      x1    x2   sd1   sd2     n hits_total hits_forth hits_back scenarios_total
#>   <dbl> <dbl> <dbl> <dbl> <int>      <int>      <int>     <int>           <int>
#> 1  3.43  5.28  1.09  2.12    70          0          0         0              11
#> 2  2.97  4.42  0.43  1.65    65          0          0         0              12
#> # ℹ 1 more variable: hit_rate <dbl>

# More steps away from the even split tend to find more hits:
df |>
  grimmer_map_total_n(
    digits_x = 2,
    digits_sd = 2,
    items_averaged = 1,
    data_type = "integer",
    dispersion = 0:10
  ) |>
  audit()
#> # A tibble: 2 × 10
#>      x1    x2   sd1   sd2     n hits_total hits_forth hits_back scenarios_total
#>   <dbl> <dbl> <dbl> <dbl> <int>      <int>      <int>     <int>           <int>
#> 1  3.43  5.28  1.09  2.12    70          0          0         0              21
#> 2  2.97  4.42  0.43  1.65    65          1          0         1              22
#> # ℹ 1 more variable: hit_rate <dbl>
```
