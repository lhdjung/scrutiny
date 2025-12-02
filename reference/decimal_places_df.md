# Count decimal places in a data frame

For every value in a column, `decimal_places_df()` counts its decimal
places. By default, it operates on all columns that are coercible to
numeric.

## Usage

``` r
decimal_places_df(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  sep = "\\."
)
```

## Arguments

- data:

  Data frame.

- cols:

  Select columns from `data` using
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, but restricted by `check_numeric_like`.

- check_numeric_like:

  Logical. If `TRUE` (the default), the function only operates on
  numeric columns and other columns coercible to numeric, as determined
  by
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md).

- sep:

  Substring that separates the mantissa from the integer part. Default
  is `"\\."`, which renders a decimal point.

## Value

Data frame. The values of the selected columns are replaced by the
numbers of their decimal places.

## See also

Wrapped functions:
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md),
[`dplyr::across()`](https://dplyr.tidyverse.org/reference/across.html).

## Examples

``` r
# Coerce all columns to string:
iris <- iris %>%
  tibble::as_tibble() %>%
  dplyr::mutate(across(everything(), as.character))

# The function will operate on all
# numeric-like columns but not on `"Species"`:
iris %>%
  decimal_places_df()
#> Warning: ! 1 column was excluded: `Species`.
#> ✖ It isn't numeric-like.
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <int>       <int>        <int>       <int> <chr>  
#>  1            1           1            1           1 setosa 
#>  2            1           0            1           1 setosa 
#>  3            1           1            1           1 setosa 
#>  4            1           1            1           1 setosa 
#>  5            0           1            1           1 setosa 
#>  6            1           1            1           1 setosa 
#>  7            1           1            1           1 setosa 
#>  8            0           1            1           1 setosa 
#>  9            1           1            1           1 setosa 
#> 10            1           1            1           1 setosa 
#> # ℹ 140 more rows

# Operate on some select columns only
# (from among the numeric-like columns):
iris %>%
  decimal_places_df(cols = starts_with("Sepal"))
#> Warning: ! 1 column was excluded: `Species`.
#> ✖ It isn't numeric-like.
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>           <int>       <int> <chr>        <chr>       <chr>  
#>  1            1           1 1.4          0.2         setosa 
#>  2            1           0 1.4          0.2         setosa 
#>  3            1           1 1.3          0.2         setosa 
#>  4            1           1 1.5          0.2         setosa 
#>  5            0           1 1.4          0.2         setosa 
#>  6            1           1 1.7          0.4         setosa 
#>  7            1           1 1.4          0.3         setosa 
#>  8            0           1 1.5          0.2         setosa 
#>  9            1           1 1.4          0.2         setosa 
#> 10            1           1 1.5          0.1         setosa 
#> # ℹ 140 more rows
```
