# Restore trailing zeros

**\[deprecated\]**

These functions are deprecated because the tests no longer take strings,
so there is no need to restore trailing zeros for them. Give the number
of decimal places as `digits_x` or `digits_sd` instead. To format
numbers for display, use `formatC(x, format = "f", digits = 2)`.

`restore_zeros()` formats numbers as strings with a given number of
decimal places, padding with zeros. By default, the target is the number
of decimal places of the value that has the most. Do not rely on this
default without checking it, since the value with the most decimal
places may itself have lost trailing zeros.

`restore_zeros_df()` applies it to the columns of a data frame, by
default all those coercible to numeric.

## Usage

``` r
restore_zeros(
  x,
  width = NULL,
  sep_in = ".",
  sep_out = sep_in,
  check_width = c("capped", "never")
)

restore_zeros_df(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  check_decimals = FALSE,
  width = NULL,
  sep_in = ".",
  sep_out = sep_in,
  check_width = c("capped", "never")
)
```

## Arguments

- x:

  Numeric, or a string coercible to numeric. A value that is not
  coercible, such as `"5%"`, becomes `NA` with a warning, and so does
  one with an exponent so large that it can't be written out, such as
  `"1e-999999"`. A blank string or `"NA"` becomes `NA` without a
  warning.

- width:

  Integer. Number of decimal places the output should have, up to
  100,000. Length 1 or the length of `x`. Default is `NULL`, the largest
  number of decimal places in `x`.

- sep_in:

  String. The separator between the integer part and the decimal places
  in the input, matched literally. Default is `"."`.

- sep_out:

  String. The separator in the output. Default is `sep_in`.

- check_width:

  String (length 1). What to do if a value has more decimal places than
  `width`? `"capped"` (the default) throws an error; `"never"` lets it
  pass.

- data:

  Data frame or matrix. Only in `restore_zeros_df()`.

- cols:

  Columns to operate on, selected with
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, restricted by `check_numeric_like`. Only in
  `restore_zeros_df()`.

- check_numeric_like:

  Logical. If `TRUE` (the default), only columns that
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md)
  accepts are processed. A column that isn't numeric-like gets a warning
  if `cols` is given and selects it, or if it is processed anyway,
  unless it is empty. Only in `restore_zeros_df()`.

- check_decimals:

  Logical. If `TRUE`, columns without any decimal places are skipped.
  Default is `FALSE`. Only in `restore_zeros_df()`.

## Value

- For `restore_zeros()`, a string vector.

- For `restore_zeros_df()`, a data frame.

## Details

Displayed numbers are often rounded, so a value may have more decimal
places than it shows. `x |> decimal_places() |> max()` gives the actual
maximum.

As in
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md),
numeric values with more than 15 significant digits cannot be padded
accurately; enter such numbers as strings.

## Examples

``` r
vec <- c(212, 75.38, 4.9625)

# By default, pad to the most decimal places in `vec`:
restore_zeros(vec)
#> Warning: `restore_zeros()` was deprecated in scrutiny 1.0.0.
#> ℹ Give the number of decimal places as `digits_x` or `digits_sd` instead. To
#>   format numbers for display, use `formatC(x, format = "f", digits = 2)`.
#> [1] "212.0000" "75.3800"  "4.9625"  

# Or to a given width:
restore_zeros(vec, width = 6)
#> [1] "212.000000" "75.380000"  "4.962500"  

iris <- tibble::as_tibble(iris)

# All numeric columns, but not the factor:
restore_zeros_df(iris)
#> Warning: `restore_zeros_df()` was deprecated in scrutiny 1.0.0.
#> ℹ Give the number of decimal places as `digits_x` or `digits_sd` instead. To
#>   format numbers for display, use `formatC(x, format = "f", digits = 2)`.
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>    <chr>        <chr>       <chr>        <chr>       <fct>  
#>  1 5.1          3.5         1.4          0.2         setosa 
#>  2 4.9          3.0         1.4          0.2         setosa 
#>  3 4.7          3.2         1.3          0.2         setosa 
#>  4 4.6          3.1         1.5          0.2         setosa 
#>  5 5.0          3.6         1.4          0.2         setosa 
#>  6 5.4          3.9         1.7          0.4         setosa 
#>  7 4.6          3.4         1.4          0.3         setosa 
#>  8 5.0          3.4         1.5          0.2         setosa 
#>  9 4.4          2.9         1.4          0.2         setosa 
#> 10 4.9          3.1         1.5          0.1         setosa 
#> # ℹ 140 more rows

# Selected columns:
restore_zeros_df(iris, starts_with("Sepal"), width = 3)
#> # A tibble: 150 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#>    <chr>        <chr>              <dbl>       <dbl> <fct>  
#>  1 5.100        3.500                1.4         0.2 setosa 
#>  2 4.900        3.000                1.4         0.2 setosa 
#>  3 4.700        3.200                1.3         0.2 setosa 
#>  4 4.600        3.100                1.5         0.2 setosa 
#>  5 5.000        3.600                1.4         0.2 setosa 
#>  6 5.400        3.900                1.7         0.4 setosa 
#>  7 4.600        3.400                1.4         0.3 setosa 
#>  8 5.000        3.400                1.5         0.2 setosa 
#>  9 4.400        2.900                1.4         0.2 setosa 
#> 10 4.900        3.100                1.5         0.1 setosa 
#> # ℹ 140 more rows
```
