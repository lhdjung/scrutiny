# Count duplicate values by column

`duplicate_count_colpair()` takes a data frame and checks each
combination of columns for duplicates. Results are presented in a
tibble, ordered by the number of duplicates.

## Usage

``` r
duplicate_count_colpair(data, ignore = NULL, show_rates = TRUE)
```

## Arguments

- data:

  Data frame.

- ignore:

  Optionally, a vector of values that should not be checked for
  duplicates.

- show_rates:

  Logical. If `TRUE` (the default), adds columns `rate_x` and `rate_y`.
  See value section. Set `show_rates` to `FALSE` for higher performance.

## Value

A tibble (data frame) with these columns –

- `x` and `y`: Each line contains a unique combination of `data`'s
  columns, stored in the `x` and `y` output columns.

- `count`: Number of "duplicates", i.e., values that are present in both
  `x` and `y`.

- `total_x`, `total_y`, `rate_x`, and `rate_y` (added by default):
  `total_x` is the number of non-missing values in the column named
  under `x`. Also, `rate_x` is the proportion of `x` values that are
  duplicated in `y`, i.e., `count / total_x`. Likewise with `total_y`
  and `rate_y`. The two `rate_*` columns will be equal unless `NA`
  values are present.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md), so
you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `duplicate_count_colpair()`. It returns a tibble with summary
statistics.

## See also

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  for a frequency table.

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  to show instances of a value next to each instance.

- `janitor::get_dupes()` to search for duplicate rows.

- [`corrr::colpair_map()`](https://corrr.tidymodels.org/reference/colpair_map.html),
  a versatile tool for pairwise column analysis which the present
  function wraps.

## Examples

``` r
# Basic usage:
mtcars %>%
  duplicate_count_colpair()
#> # A tibble: 55 × 7
#>    x     y     count total_x total_y rate_x rate_y
#>    <chr> <chr> <int>   <int>   <int>  <dbl>  <dbl>
#>  1 cyl   carb     32      32      32 1      1     
#>  2 vs    am       32      32      32 1      1     
#>  3 gear  carb     27      32      32 0.844  0.844 
#>  4 vs    carb     14      32      32 0.438  0.438 
#>  5 am    carb     13      32      32 0.406  0.406 
#>  6 cyl   gear     11      32      32 0.344  0.344 
#>  7 drat  wt        3      32      32 0.0938 0.0938
#>  8 mpg   qsec      2      32      32 0.0625 0.0625
#>  9 drat  gear      1      32      32 0.0312 0.0312
#> 10 drat  carb      1      32      32 0.0312 0.0312
#> # ℹ 45 more rows

# Summaries with `audit()`:
mtcars %>%
  duplicate_count_colpair() %>%
  audit()
#> # A tibble: 5 × 8
#>   term       mean    sd median   min   max na_count na_rate
#>   <chr>     <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 count    2.47   7.38       0     0    32        0       0
#> 2 total_x 32      0         32    32    32        0       0
#> 3 total_y 32      0         32    32    32        0       0
#> 4 rate_x   0.0773 0.231      0     0     1        0       0
#> 5 rate_y   0.0773 0.231      0     0     1        0       0
```
