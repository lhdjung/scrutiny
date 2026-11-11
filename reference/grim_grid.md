# GRIM results on a grid of sample sizes and means

**\[experimental\]**

`grim_grid()` tests every combination of sample size and fractional part
of a mean with GRIM, for one number of decimal places and one rounding
method. It is meant for studying GRIM's properties directly.
[`grim_plot_grid()`](https://lhdjung.github.io/scrutiny/reference/grim_plot_grid.md)
draws the inconsistent cells as its background raster.

## Usage

``` r
grim_grid(digits_x, data_type, rounding = "up_or_down", threshold = 5)
```

## Arguments

- digits_x:

  Integer. Number of decimal places, from 1 to 3.

- data_type:

  String. `"integer"` if the values are whole numbers, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).
  Under `"continuous"`, every cell is consistent, and under `NA`, the
  inconsistent ones are `NA`.

- rounding, threshold:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

A tibble with one row per cell of the grid and the columns `frac` (the
fractional part of a mean), `n` (the sample size), and `consistency`
(the GRIM result).

## Details

The grid covers sample sizes from 1 to `10^digits_x`, where GRIM stops
ruling out any mean, and fractional parts in steps of `1 / 10^digits_x`,
without zero. All fractional parts of means are positive, so there is no
`symmetric` argument.

For percentages, use the number of decimal places of the proportion,
which is two more: `grim_grid(2)` for whole percentages, as in
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
with `x_meaning = "percentage"`.

Grids for one or two decimal places are precomputed for every rounding
method that has no `threshold`. All others are computed on demand, which
takes some 20 to 30 seconds for three decimal places. Four or more would
mean a grid of 100 million cells, so `digits_x` is at most 3.

## Examples

``` r
grim_grid(digits_x = 1, data_type = "integer")
#> # A tibble: 90 × 3
#>     frac     n consistency
#>    <dbl> <int> <lgl>      
#>  1   0.1     1 FALSE      
#>  2   0.2     1 FALSE      
#>  3   0.3     1 FALSE      
#>  4   0.4     1 FALSE      
#>  5   0.5     1 FALSE      
#>  6   0.6     1 FALSE      
#>  7   0.7     1 FALSE      
#>  8   0.8     1 FALSE      
#>  9   0.9     1 FALSE      
#> 10   0.1     2 FALSE      
#> # ℹ 80 more rows

# Share of means ruled out per sample size:
grim_grid(digits_x = 2, data_type = "integer") |>
  dplyr::summarise(share_inconsistent = mean(!consistency), .by = n)
#> # A tibble: 100 × 2
#>        n share_inconsistent
#>    <int>              <dbl>
#>  1     1              1    
#>  2     2              0.990
#>  3     3              0.980
#>  4     4              0.970
#>  5     5              0.960
#>  6     6              0.949
#>  7     7              0.939
#>  8     8              0.889
#>  9     9              0.919
#> 10    10              0.909
#> # ℹ 90 more rows
```
