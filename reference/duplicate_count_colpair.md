# Count duplicate values by column pair

`duplicate_count_colpair()` counts, for every pair of columns in a data
frame, the values that occur in both. Results are ordered by that count,
in descending order.

## Usage

``` r
duplicate_count_colpair(data, ignore = NULL, show_rates = TRUE)
```

## Arguments

- data:

  Data frame or matrix with at least two columns. Blank strings, `"NA"`,
  and `"NaN"` count as missing.

- ignore:

  Optionally, a vector of values not to count.

- show_rates:

  Logical. If `TRUE` (the default), add `total_x`, `total_y`, `rate_x`,
  and `rate_y`; see *Value*. `FALSE` is faster.

## Value

A tibble with these columns:

- `x`, `y`: a pair of columns of `data`, one pair per row.

- `count`: the number of values of `x` that also occur in `y`. Missing
  and ignored values are not counted.

- `total_x`, `total_y`: the number of values in `x` and `y` that are
  neither missing nor ignored.

- `rate_x`: the proportion of `x` values that occur in `y`, i.e.,
  `count / total_x`, or `NA` if `total_x` is 0.

- `rate_y`: the same for `y`. It differs from `count / total_y` if a
  value repeats within a column.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`duplicate_count_colpair()` output returns summary statistics for the
numeric columns.

## See also

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  for a frequency table.

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  for counts next to each value.

- `janitor::get_dupes()` for duplicate rows.

## Examples

``` r
mtcars |>
  duplicate_count_colpair()
#> # A tibble: 55 × 7
#>    x     y     count total_x total_y rate_x rate_y
#>    <chr> <chr> <int>   <int>   <int>  <dbl>  <dbl>
#>  1 cyl   carb     32      32      32 1      0.375 
#>  2 vs    am       32      32      32 1      1     
#>  3 gear  carb     27      32      32 0.844  0.406 
#>  4 vs    carb     14      32      32 0.438  0.219 
#>  5 am    carb     13      32      32 0.406  0.219 
#>  6 cyl   gear     11      32      32 0.344  0.375 
#>  7 drat  wt        3      32      32 0.0938 0.0625
#>  8 mpg   qsec      2      32      32 0.0625 0.0625
#>  9 drat  gear      1      32      32 0.0312 0.469 
#> 10 drat  carb      1      32      32 0.0312 0.0938
#> # ℹ 45 more rows

mtcars |>
  duplicate_count_colpair() |>
  audit()
#> # A tibble: 5 × 8
#>   term       mean    sd median   min   max na_count na_rate
#>   <chr>     <dbl> <dbl>  <dbl> <dbl> <dbl>    <int>   <dbl>
#> 1 count    2.47   7.38       0     0    32        0       0
#> 2 total_x 32      0         32    32    32        0       0
#> 3 total_y 32      0         32    32    32        0       0
#> 4 rate_x   0.0773 0.231      0     0     1        0       0
#> 5 rate_y   0.0597 0.171      0     0     1        0       0
```
