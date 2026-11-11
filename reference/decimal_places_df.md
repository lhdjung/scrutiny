# Count decimal places in a data frame

**\[deprecated\]**

`decimal_places_df()` is deprecated because it replaces the values by
their counts, while the tests need both. Use
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
in
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
instead, as in `mutate(digits_x = decimal_places(x))`.

`decimal_places_df()` replaces every value in the selected columns by
its number of decimal places. By default, it operates on all columns
coercible to numeric.

## Usage

``` r
decimal_places_df(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  sep = "."
)
```

## Arguments

- data:

  Data frame.

- cols:

  Columns to operate on, selected with
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, restricted by `check_numeric_like`.

- check_numeric_like:

  Logical. If `TRUE` (the default), only columns that
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  accepts are processed. A column that isn't numeric-like gets a warning
  if `cols` is given and selects it, or if it is processed anyway,
  unless it is empty.

- sep:

  String. The separator between the integer part and the decimal places,
  matched literally. Default is `"."`.

## Value

Data frame with the selected columns replaced by their counts of decimal
places.

## See also

[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md).

## Examples

``` r
iris <- iris |>
  tibble::as_tibble() |>
  dplyr::mutate(across(everything(), as.character))

# All numeric-like columns, silently leaving out `Species`:
iris |>
  decimal_places_df()
#> Warning: `decimal_places_df()` was deprecated in scrutiny 1.0.0.
#> ℹ Please use `decimal_places()` instead.
#> ℹ Use it in `dplyr::mutate()`, as in `mutate(digits_x = decimal_places(x))`,
#>   which keeps the values.
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

# Selected columns only:
iris |>
  decimal_places_df(cols = starts_with("Sepal"))
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
