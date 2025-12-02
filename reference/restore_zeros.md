# Restore trailing zeros

`restore_zeros()` takes a vector with values that might have lost
trailing zeros, most likely from being registered as numeric. It turns
each value into a string and adds trailing zeros until the mantissa hits
some limit.

The default for that limit is the number of digits in the longest
mantissa of the vector's values. The length of the integer part plays no
role.

Don't rely on the default limit without checking: The original width
could have been larger because the longest extant mantissa might itself
have lost trailing zeros.

`restore_zeros_df()` is a variant for data frames. It wraps
`restore_zeros()` and, by default, applies it to all columns that are
coercible to numeric.

## Usage

``` r
restore_zeros(x, width = NULL, sep_in = "\\.", sep_out = sep_in)

restore_zeros_df(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  check_decimals = FALSE,
  width = NULL,
  sep_in = "\\.",
  sep_out = NULL,
  ...
)
```

## Arguments

- x:

  Numeric (or string coercible to numeric). Vector of numbers that might
  have lost trailing zeros.

- width:

  Integer. Number of decimal places the mantissas should have, including
  the restored zeros. If specified, `width` needs to be length 1 or the
  same length as `x`. Default is `NULL`, in which case the number of
  characters in the longest mantissa will be used instead.

- sep_in:

  Substring that separates the input's mantissa from its integer part.
  Default is `"\\."`, which renders a decimal point.

- sep_out:

  Substring that will be returned in the output to separate the mantissa
  from the integer part. By default, `sep_out` is the same as `sep_in`.

- data:

  Data frame or matrix. Only in `restore_zeros_df()`, and instead of
  `x`.

- cols:

  Only in `restore_zeros_df()`. Select columns from `data` using
  [tidyselect](https://tidyselect.r-lib.org/reference/language.html).
  Default is `everything()`, which selects all columns that pass the
  test of `check_numeric_like`.

- check_numeric_like:

  Logical. Only in `restore_zeros_df()`. If `TRUE` (the default), the
  function will skip columns that are not numeric or coercible to
  numeric, as determined by
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md).

- check_decimals:

  Logical. Only in `restore_zeros_df()`. If set to `TRUE`, the function
  will skip columns where no values have any decimal places. Default is
  `FALSE`.

- ...:

  Only in `restore_zeros_df()`. These dots must be empty.

## Value

- For `restore_zeros()`, a string vector. At least some of the strings
  will have newly restored zeros, unless (1) all input values had the
  same number of decimal places, and (2) `width` was not specified as a
  number greater than that single number of decimal places.

- For `restore_zeros_df()`, a data frame.

## Details

These functions exploit the fact that groups of summary values such as
means or percentages are often reported to the same number of decimal
places. If such a number is known but values were not entered as
strings, trailing zeros will be lost. In this case, `restore_zeros()` or
`restore_zeros_df()` will be helpful to prepare data for consistency
testing functions such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
or
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md).

## Displaying decimal places

You might not see all decimal places of numeric values in a vector, and
consequently wonder if `restore_zeros()`, when applied to the vector,
adds too many zeros. That is because displayed numbers, unlike stored
numbers, are often rounded.

For a vector `x`, you can count the characters of the longest mantissa
from among its values like this:

`x %>% decimal_places() %>% max()`

## See also

Wrapped functions: [`sprintf()`](https://rdrr.io/r/base/sprintf.html).

## Examples

``` r
# By default, the target width is that of
# the longest mantissa:
vec <- c(212, 75.38, 4.9625)
vec %>%
  restore_zeros()
#> [1] "212.0000" "75.3800"  "4.9625"  

# Alternatively, supply a number via `width`:
vec %>%
  restore_zeros(width = 6)
#> [1] "212.000000" "75.380000"  "4.962500"  

# For better printing:
iris <- tibble::as_tibble(iris)

# Apply `restore_zeros()` to all numeric
# columns, but not to the factor column:
iris %>%
  restore_zeros_df()
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

# Select columns as in `dplyr::select()`:
iris %>%
  restore_zeros_df(starts_with("Sepal"), width = 3)
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
