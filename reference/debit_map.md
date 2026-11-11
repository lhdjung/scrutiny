# DEBIT-test many cases at once

`debit_map()` applies
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) to
every row of a data frame with means, standard deviations, and sample
sizes of binary data.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics and
[`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md)
to visualize it.

## Usage

``` r
debit_map(
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
  show_rec = FALSE
)
```

## Arguments

- data:

  Data frame with columns `x`, `sd`, and `n`, and optionally
  `data_type`, `digits_x`, and `digits_sd`. Any other columns are
  returned alongside the results.

- digits_x, digits_sd:

  Integer. The number of decimal places in `x` and `sd`, including
  trailing zeros. There are no defaults, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for the whole
  column or one number per row of `data`. If left out, they are taken
  from `digits_x` and `digits_sd` columns of `data`.

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
  columns; see *Value* in `debit_map()`. Default is `FALSE`.

## Value

A tibble with these columns:

- `x`, `sd`, `n`: the inputs.

- `data_type`: the kind of data, as given.

- `digits_x`, `digits_sd`: the numbers of decimal places, as given.

- `consistency`: DEBIT consistency of `x`, `sd`, and `n`. `NA` for a
  case that cannot be decided, such as one with a missing value.

- Any other columns from `data`.

If `show_rec` is `TRUE`, further columns right after `consistency` hold
the reconstructed values: the bounds of `sd` and `x` (`sd_lower`,
`sd_upper`, `x_lower`, `x_upper`) with `sd_incl_lower` and
`sd_incl_upper` saying whether the SD bounds are inclusive; and
`sum_lower` and `sum_upper`, the least and the greatest number of ones
among `n` values whose mean would be reported as `x`. Under
`formula = "exact"`, a value set is consistent exactly if some `k` from
`sum_lower` to `sum_upper` has an SD within the bounds of `sd`.

The tibble has the class `scrutiny_debit_map`, which is recognized by
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) and
[`debit_plot()`](https://lhdjung.github.io/scrutiny/reference/debit_plot.md).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`debit_map()` output returns a tibble with one row and these columns:

1.  `all_cases`: total number of value sets.

2.  `incon_cases`: number of DEBIT-inconsistent value sets.

3.  `emp_incon`: proportion of DEBIT-inconsistent value sets.

4.  `mean_x`: mean of the `x` values.

5.  `mean_sd`: mean of the `sd` values.

6.  `distinct_n`: number of distinct `n` values.

## References

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## See also

[`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
and
[`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md)
to explain each row's result in plain words.

## Examples

``` r
# Call `debit_map()` on binary summary data such as these:
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

# The `consistency` column shows whether the
# values to its left are DEBIT-consistent:
pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer")
#> # A tibble: 7 × 7
#> # DEBIT: default settings
#>       x    sd     n data_type digits_x digits_sd consistency
#>   <dbl> <dbl> <int> <chr>        <int>     <int> <lgl>      
#> 1  0.53  0.5   1683 integer          2         2 TRUE       
#> 2  0.44  0.5   1683 integer          2         2 TRUE       
#> 3  0.77  0.42  1683 integer          2         2 TRUE       
#> 4  0.19  0.35  1683 integer          2         2 FALSE      
#> 5  0.34  0.47  1683 integer          2         2 TRUE       
#> 6  0.93  0.25  1683 integer          2         2 TRUE       
#> 7  0.12  0.33  1683 integer          2         2 TRUE       

# Get test summaries with `audit()`:
pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  audit()
#> # A tibble: 1 × 6
#>   all_cases incon_cases emp_incon mean_x mean_sd distinct_n
#>       <int>       <int>     <dbl>  <dbl>   <dbl>      <int>
#> 1         7           1     0.143  0.474   0.403          1

# Explain the results in plain words:
pigs3 |>
  debit_map(digits_x = 2, digits_sd = 2, data_type = "integer") |>
  explain_print(1:2)
#> DEBIT test: x = 0.53, sd = 0.50, n = 1683, data_type = "integer"
#> 
#> DEBIT is about binary data, where every value is 0 or 1. The mean is then the
#> share of ones, and the SD follows from the number of ones and the sample size.
#> 
#> The mean was reported as 0.53, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 0.525 to 0.535.
#> 
#> With 1683 values, the mean is the number of ones divided by 1683, so that
#> number must be from 0.525 * 1683 = 883.575 to 0.535 * 1683 = 900.405. It has to
#> be an integer, so it can be anything from 884 to 900.
#> 
#> The SD was reported as 0.50, with 2 decimal places. Before rounding, it could
#> have been anything from 0.495 to 0.505.
#> 
#> With k ones among 1683 values, the SD is sqrt(k * (1683 - k) / (1683 * 1682)).
#> For example, for k = 900, that is sqrt(900 * 783 / 2830806) = 0.4989, which is
#> in that range.
#> 
#> ==> Consistent
#> 
#> DEBIT test: x = 0.44, sd = 0.50, n = 1683, data_type = "integer"
#> 
#> DEBIT is about binary data, where every value is 0 or 1. The mean is then the
#> share of ones, and the SD follows from the number of ones and the sample size.
#> 
#> The mean was reported as 0.44, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 0.435 to 0.445.
#> 
#> With 1683 values, the mean is the number of ones divided by 1683, so that
#> number must be from 0.435 * 1683 = 732.105 to 0.445 * 1683 = 748.935. It has to
#> be an integer, so it can be anything from 733 to 748.
#> 
#> The SD was reported as 0.50, with 2 decimal places. Before rounding, it could
#> have been anything from 0.495 to 0.505.
#> 
#> With k ones among 1683 values, the SD is sqrt(k * (1683 - k) / (1683 * 1682)).
#> For example, for k = 748, that is sqrt(748 * 935 / 2830806) = 0.4971, which is
#> in that range.
#> 
#> ==> Consistent
#> 
```
