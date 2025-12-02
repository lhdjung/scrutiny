# GRIM-test many cases at once

Call `grim_map()` to GRIM-test any number of combinations of
mean/proportion, sample size, and number of items. Mapping function for
GRIM-testing.

Set `percent` to `TRUE` if the `x` values are percentages. This will
convert `x` values to decimals and adjust the decimal count accordingly.

Display intermediary numbers from GRIM-testing in columns by setting
`show_rec` to `TRUE`.

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results.

## Usage

``` r
grim_map(
  data,
  items = 1,
  merge_items = TRUE,
  percent = FALSE,
  x = NULL,
  n = NULL,
  show_rec = FALSE,
  show_prob = deprecated(),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5,
  testables_only = FALSE,
  extra = Inf
)
```

## Arguments

- data:

  Data frame with columns `x`, `n`, and optionally `items` (see
  documentation for
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). By
  default, any other columns in `data` will be returned alongside GRIM
  test results (see `extra` below).

- items:

  Integer. If there is no `items` column in `data`, this specifies the
  number of items composing the `x` values. Default is 1, the most
  common case.

- merge_items:

  Logical. If `TRUE` (the default), there will be no `items` column in
  the output. Instead, values from an `items` column or argument will be
  multiplied with values in the `n` column. This is only for
  presentation and does not affect test results.

- percent:

  Logical. Set `percent` to `TRUE` if the `x` values are percentages.
  This will convert them to decimal numbers and adjust the decimal count
  (i.e., increase it by 2). It also affects the `ratio` column. Default
  is `FALSE`.

- x, n:

  Optionally, specify these arguments as column names in `data`.

- show_rec:

  Logical. If set to `TRUE`, the reconstructed numbers from GRIM-testing
  are shown as columns. See section *Reconstructed numbers* below.
  Default is `FALSE`.

- show_prob:

  **\[deprecated\]** Logical. No longer supported: now, there is always
  a `probability` column. (It replaces the earlier `ratio` column.)

- rounding, threshold, symmetric, tolerance:

  Further parameters of GRIM-testing; see documentation for
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- testables_only:

  Logical. If `testables_only` is set to `TRUE`, only GRIM-testable
  cases (i.e., those with a positive GRIM ratio) are included. Default
  is `FALSE`.

- extra:

  String or integer. The other column(s) from `data` to be returned in
  the output tibble alongside test results, referenced by their name(s)
  or number(s). Default is `Inf`, which returns all columns. To return
  none of them, set `extra` to 0.

## Value

A tibble with these columns –

- `x`, `n`: the inputs.

- `consistency`: GRIM consistency of `x`, `n`, and `items`.

- `probability`: the probability of GRIM inconsistency; see
  [`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md).

- `<extra>`: any columns from `data` other than `x`, `n`, and `items`.

  The tibble has the `scr_grim_map` class, which is recognized by the
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
  generic.

## Reconstructed numbers

If `show_rec` is set to `TRUE`, the output includes the following
additional columns:

- `rec_sum`: the sum total from which the mean or proportion was
  ostensibly derived.

- `rec_x_upper`: the upper reconstructed `x` value.

- `rec_x_lower`: the lower reconstructed `x` value.

- `rec_x_upper_rounded`: the rounded `rec_x_upper` value.

- `rec_x_lower_rounded`: the rounded `rec_x_lower` value.

With the default for `rounding`, `"up_or_down"`, each of the last two
columns is replaced by two columns that specify the rounding procedures
(i.e., `"_up"` and `"_down"`).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md), so
you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `grim_map()` to get a summary of `grim_map()`'s results. It is
a tibble with one row and these columns –

1.  `incons_cases`: number of GRIM-inconsistent value sets.

2.  `all_cases`: total number of value sets.

3.  `incons_rate`: proportion of GRIM-inconsistent value sets.

4.  `mean_grim_prob`: average probability of GRIM inconsistency.

5.  `incons_to_prob`: ratio of `incons_rate` to `mean_grim_prob`.

6.  `testable_cases`: number of GRIM-testable value sets (i.e., those
    with a positive `probability`).

7.  `testable_rate`: proportion of GRIM-testable value sets.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## Examples

``` r
# Use `grim_map()` on data like these:
pigs1
#> # A tibble: 12 × 2
#>    x         n
#>    <chr> <dbl>
#>  1 7.22     32
#>  2 4.74     25
#>  3 5.23     29
#>  4 2.57     24
#>  5 6.77     27
#>  6 2.68     28
#>  7 7.01     29
#>  8 7.38     26
#>  9 3.14     27
#> 10 6.89     31
#> 11 5.00     25
#> 12 0.24     28

# The `consistency` column shows
# whether the values to its left
# are GRIM-consistent:
pigs1 %>%
  grim_map()
#> # A tibble: 12 × 4
#>    x         n consistency probability
#>    <chr> <dbl> <lgl>             <dbl>
#>  1 7.22     32 TRUE               0.68
#>  2 4.74     25 FALSE              0.75
#>  3 5.23     29 FALSE              0.71
#>  4 2.57     24 FALSE              0.76
#>  5 6.77     27 FALSE              0.73
#>  6 2.68     28 TRUE               0.72
#>  7 7.01     29 FALSE              0.71
#>  8 7.38     26 TRUE               0.74
#>  9 3.14     27 FALSE              0.73
#> 10 6.89     31 FALSE              0.69
#> 11 5.00     25 TRUE               0.75
#> 12 0.24     28 FALSE              0.72

# Display intermediary numbers from
# GRIM-testing with `show_rec = TRUE`:
pigs1 %>%
  grim_map(show_rec = TRUE)
#> # A tibble: 12 × 11
#>    x         n consistency probability rec_sum rec_x_upper rec_x_lower
#>    <chr> <dbl> <lgl>             <dbl>   <dbl>       <dbl>       <dbl>
#>  1 7.22     32 TRUE               0.68  231.         7.25        7.22 
#>  2 4.74     25 FALSE              0.75  118.         4.76        4.72 
#>  3 5.23     29 FALSE              0.71  152.         5.24        5.21 
#>  4 2.57     24 FALSE              0.76   61.7        2.58        2.54 
#>  5 6.77     27 FALSE              0.73  183.         6.78        6.74 
#>  6 2.68     28 TRUE               0.72   75.0        2.71        2.68 
#>  7 7.01     29 FALSE              0.71  203.         7.03        7.00 
#>  8 7.38     26 TRUE               0.74  192.         7.38        7.35 
#>  9 3.14     27 FALSE              0.73   84.8        3.15        3.11 
#> 10 6.89     31 FALSE              0.69  214.         6.90        6.87 
#> 11 5.00     25 TRUE               0.75  125          5.00        5.00 
#> 12 0.24     28 FALSE              0.72    6.72       0.250       0.214
#> # ℹ 4 more variables: rec_x_upper_rounded_up <dbl>,
#> #   rec_x_upper_rounded_down <dbl>, rec_x_lower_rounded_up <dbl>,
#> #   rec_x_lower_rounded_down <dbl>

# Get summaries with `audit()`:
pigs1 %>%
  grim_map() %>%
  audit()
#> # A tibble: 1 × 7
#>   incons_cases all_cases incons_rate mean_grim_prob incons_to_prob
#>          <int>     <int>       <dbl>          <dbl>          <dbl>
#> 1            8        12       0.667          0.724          0.921
#> # ℹ 2 more variables: testable_cases <int>, testable_rate <dbl>
```
