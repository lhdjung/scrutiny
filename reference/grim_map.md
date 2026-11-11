# GRIM-test many cases at once

`grim_map()` applies
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) to
every row of a data frame with means (or percentages) and sample sizes.

Set `x_meaning` to `"percentage"` if `x` contains percentages, and
`show_rec` to `TRUE` to see the reconstructed values behind each result.

Call these functions on the output of `grim_map()`:

- [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) for
  summary statistics.

- [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
  to visualize the results.

- [`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
  to explain each result in plain words.

## Usage

``` r
grim_map(
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
  show_rec = FALSE
)
```

## Arguments

- data:

  Data frame with columns `x` and `n`, and optionally `items_averaged`,
  `data_type`, and `digits_x`. Any other columns are returned alongside
  the results.

- digits_x:

  Integer. The number of decimal places in `x`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`). Either a single number for the whole column or one
  number per row of `data`. If left out, it is taken from a `digits_x`
  column of `data`.

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
  `grim_map()`. The parts record different settings, so they can't be
  combined with
  [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
  See *Defaults* in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- rounding, threshold, symmetric:

  Passed on to
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
  whose section *Defaults* explains the defaults.

- show_rec:

  Logical. If `TRUE`, the reconstructed numbers are returned as columns;
  see *Reconstructed numbers* in `grim_map()`. Default is `FALSE`.

## Value

A tibble with these columns:

- `x`, `n`: the inputs.

- `items_averaged`: the number of items averaged into each score.

- `data_type`: the kind of data, as given.

- `digits_x`: the number of decimal places in `x`, as given.

- `consistency`: GRIM consistency of `x`, `n`, and `items_averaged`.
  `NA` for a case that cannot be decided, such as one with a missing
  value.

- `exp_incon`: the probability of GRIM inconsistency; see
  [`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md).

- Any other columns from `data`.

  The tibble has the class `scrutiny_grim_map`, which is recognized by
  [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) and
  [`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md).

## Reconstructed numbers

With `show_rec = TRUE`, the output has these further columns, the same
ones for every rounding method:

- `rec_sum`: the product of `x` and `n` (times `items_averaged`), i.e.,
  the sum total implied by the reported mean. It is generally not a
  whole number.

- `sum_lower`, `sum_upper`: the least and the greatest whole-number sum
  total that would have been reported as `x`. A value set is consistent
  exactly if `sum_lower` is not greater than `sum_upper`. If it is
  inconsistent, the two are the whole numbers on either side of
  `rec_sum`, so their gap is always 1 and does not measure how far off
  the reported values are.

- `rec_x_upper`, `rec_x_lower`: the two achievable means closest to `x`,
  i.e., `rec_sum` rounded up and down, divided by `n` (times
  `items_averaged`).

With `x_meaning = "percentage"`, `rec_sum` is `x / 100` times `n` (times
`items_averaged`), and `rec_x_upper` and `rec_x_lower` are percentages
like `x`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`grim_map()` output returns a tibble with one row and these columns:

1.  `all_cases`: total number of value sets.

2.  `testable_cases`: number of value sets with a positive `exp_incon`.

3.  `incon_cases`: number of GRIM-inconsistent value sets.

4.  `emp_incon`: proportion of GRIM-inconsistent value sets.

5.  `exp_incon`: mean probability of GRIM inconsistency.

6.  `ratio`: ratio of `emp_incon` to `exp_incon`, both computed over the
    value sets that could be decided.

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

# The `consistency` column shows whether the
# values to its left are GRIM-consistent:
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 12 × 7
#> # GRIM: default settings
#>        x     n items_averaged data_type digits_x consistency exp_incon
#>    <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl>
#>  1  7.22    32              1 integer          2 TRUE             0.64
#>  2  4.74    25              1 integer          2 FALSE            0.75
#>  3  5.23    29              1 integer          2 FALSE            0.71
#>  4  2.57    24              1 integer          2 FALSE            0.72
#>  5  6.77    27              1 integer          2 FALSE            0.73
#>  6  2.68    28              1 integer          2 TRUE             0.72
#>  7  7.01    29              1 integer          2 FALSE            0.71
#>  8  7.38    26              1 integer          2 TRUE             0.74
#>  9  3.14    27              1 integer          2 FALSE            0.73
#> 10  6.89    31              1 integer          2 FALSE            0.69
#> 11  5       25              1 integer          2 TRUE             0.75
#> 12  0.24    28              1 integer          2 FALSE            0.72

# Show the reconstructed numbers:
pigs1 |>
  grim_map(
    digits_x = 2,
    items_averaged = 1,
    data_type = "integer",
    show_rec = TRUE
  )
#> # A tibble: 12 × 12
#> # GRIM: show_rec = TRUE
#>        x     n items_averaged data_type digits_x consistency exp_incon rec_sum
#>    <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl>   <dbl>
#>  1  7.22    32              1 integer          2 TRUE             0.64  231.  
#>  2  4.74    25              1 integer          2 FALSE            0.75  118.  
#>  3  5.23    29              1 integer          2 FALSE            0.71  152.  
#>  4  2.57    24              1 integer          2 FALSE            0.72   61.7 
#>  5  6.77    27              1 integer          2 FALSE            0.73  183.  
#>  6  2.68    28              1 integer          2 TRUE             0.72   75.0 
#>  7  7.01    29              1 integer          2 FALSE            0.71  203.  
#>  8  7.38    26              1 integer          2 TRUE             0.74  192.  
#>  9  3.14    27              1 integer          2 FALSE            0.73   84.8 
#> 10  6.89    31              1 integer          2 FALSE            0.69  214.  
#> 11  5       25              1 integer          2 TRUE             0.75  125   
#> 12  0.24    28              1 integer          2 FALSE            0.72    6.72
#> # ℹ 4 more variables: sum_lower <dbl>, sum_upper <dbl>, rec_x_upper <dbl>,
#> #   rec_x_lower <dbl>

# Get summaries with `audit()`:
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  audit()
#> # A tibble: 1 × 6
#>   all_cases testable_cases incon_cases emp_incon exp_incon ratio
#>       <int>          <int>       <int>     <dbl>     <dbl> <dbl>
#> 1        12             12           8     0.667     0.718 0.929

# Explain the results in plain words:
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  explain_print(1:2)
#> GRIM test: x = 7.22, n = 32, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 7.22, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 7.215 to 7.225.
#> 
#> The mean of 32 integers is their sum divided by 32. So the sum must be from
#> 7.215 * 32 = 230.88 to 7.225 * 32 = 231.2.
#> 
#> A sum of integers is itself an integer, and 231 is in this range:
#> 231 / 32 = 7.2188, which rounds to 7.22.
#> 
#> ==> Consistent
#> 
#> GRIM test: x = 4.74, n = 25, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 4.74, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 4.735 to 4.745.
#> 
#> The mean of 25 integers is their sum divided by 25. So the sum must be from
#> 4.735 * 25 = 118.375 to 4.745 * 25 = 118.625.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 118 and 119. They give 118 / 25 = 4.72 and
#> 119 / 25 = 4.76, which round to 4.72 and 4.76, not to 4.74.
#> 
#> ==> Inconsistent
#> 

# A table of means and percentages, tested in parts:
df <- tibble::tibble(
  x = c(5.19, 71),
  n = c(32, 43),
  is_percent = c(FALSE, TRUE)
)

df |>
  dplyr::filter(!is_percent) |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 1 × 8
#> # GRIM: default settings
#>       x     n items_averaged data_type digits_x consistency exp_incon is_percent
#>   <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl> <lgl>     
#> 1  5.19    32              1 integer          2 TRUE             0.64 FALSE     

df |>
  dplyr::filter(is_percent) |>
  grim_map(
    digits_x = 0,
    items_averaged = 1,
    data_type = "integer",
    x_meaning = "percentage"
  )
#> # A tibble: 1 × 8
#> # GRIM: x_meaning = "percentage"
#>       x     n items_averaged data_type digits_x consistency exp_incon is_percent
#>   <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl> <lgl>     
#> 1    71    43              1 integer          0 FALSE            0.57 TRUE      
```
