# GRIM-test with hypothetical group sizes

Some studies report group means but only the total sample size, not the
group sizes. For two groups, `grim_map_total_n()` tests hypothetical
splits of the total: it starts from an even split, moves one participant
at a time from one group to the other, and GRIM-tests both means against
both group sizes, in both pairings.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
grim_map_total_n(
  data,
  digits_x,
  x1 = NULL,
  x2 = NULL,
  n = NULL,
  ...,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  show_rec = FALSE,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL
)
```

## Arguments

- data:

  Data frame with numeric columns `x1`, `x2`, and `n`: the two group
  means (or percentages) and the total sample size. Which mean is `x1`
  does not matter, since both pairings with the group sizes are tested.

- digits_x:

  Integer. The number of decimal places in `x1` and `x2`, including
  trailing zeros. There is no default, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for both groups,
  or two numbers, one for `x1` and one for `x2`, which apply to every
  row of `data`. If left out, it is taken from a `digits_x` column of
  `data`, with one number per row for both groups. If the decimal places
  differ both by group and by row, call the function on each set of rows
  separately and
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  each output.

- x1, x2, n:

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

- dispersion:

  Numeric. Steps up and down from half the total `n`. Default is `0:5`,
  i.e., the even split and five steps away from it.

- n_min:

  Numeric. Minimal group size. Default is 1.

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

A tibble with these columns:

- `x`: the reported means, in pairs of rows.

- `n`: the hypothetical group sizes, with `n_change` recording the steps
  from half the total.

- `both_consistent`: whether both means of a pair are consistent with
  their hypothetical group sizes.

- `case`: the row number in `data`.

- `dir`: `"forth"` where `x2` is paired with the larger group size,
  `"back"` where `x1` is. The split of an even total into two equal
  groups is the same scenario either way, so it appears under `"forth"`
  only.

- The other columns of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  output, and the other columns of `data`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grim_map_total_n()` output returns a tibble with one row per case and
these columns:

- `x1`, `x2`, and `n`: the reported values.

- `hits_total`: the number of scenarios in which both groups are
  GRIM-consistent, summing `hits_forth` and `hits_back`.

- `hits_forth`: the number of such scenarios with `x2` paired with the
  larger group.

- `hits_back`: the same with `x1` paired with the larger group.

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

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which created this function from
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

## Examples

``` r
df <- tibble::tribble(
  ~x1,  ~x2,  ~n,
  3.43, 5.28, 90,
  2.97, 4.42, 103
)

grim_map_total_n(df, digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 46 × 11
#> # GRIM: default settings
#>        x     n n_change items_averaged data_type digits_x consistency
#>    <dbl> <int>    <int>          <int> <chr>        <int> <lgl>      
#>  1  3.43    45        0              1 integer          2 FALSE      
#>  2  5.28    45        0              1 integer          2 FALSE      
#>  3  3.43    44       -1              1 integer          2 TRUE       
#>  4  5.28    46        1              1 integer          2 TRUE       
#>  5  3.43    43       -2              1 integer          2 FALSE      
#>  6  5.28    47        2              1 integer          2 TRUE       
#>  7  3.43    42       -3              1 integer          2 TRUE       
#>  8  5.28    48        3              1 integer          2 FALSE      
#>  9  3.43    41       -4              1 integer          2 FALSE      
#> 10  5.28    49        4              1 integer          2 FALSE      
#> # ℹ 36 more rows
#> # ℹ 4 more variables: both_consistent <lgl>, exp_incon <dbl>, case <int>,
#> #   dir <fct>

# `hits_total` counts the splits in which both means are consistent:
df |>
  grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  audit()
#> # A tibble: 2 × 8
#>      x1    x2     n hits_total hits_forth hits_back scenarios_total hit_rate
#>   <dbl> <dbl> <int>      <int>      <int>     <int>           <int>    <dbl>
#> 1  3.43  5.28    90          3          2         1              11    0.273
#> 2  2.97  4.42   103          0          0         0              12    0    

# More steps away from the even split tend to find more hits:
df |>
  grim_map_total_n(
    digits_x = 2,
    items_averaged = 1,
    data_type = "integer",
    dispersion = 0:10
  ) |>
  audit()
#> # A tibble: 2 × 8
#>      x1    x2     n hits_total hits_forth hits_back scenarios_total hit_rate
#>   <dbl> <dbl> <int>      <int>      <int>     <int>           <int>    <dbl>
#> 1  3.43  5.28    90          6          3         3              21   0.286 
#> 2  2.97  4.42   103          2          0         2              22   0.0909
```
