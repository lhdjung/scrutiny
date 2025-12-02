# Reconstruct rounding bounds

`unround()` takes a rounded number and returns the range of the original
value: lower and upper bounds for the hypothetical earlier number that
was later rounded to the input number. It also displays a range with
inequation signs, showing whether the bounds are inclusive or not.

By default, the presumed rounding method is rounding up (or down) from
5. See the `Rounding` section for other methods.

## Usage

``` r
unround(x, rounding = "up_or_down", threshold = 5, digits = NULL)
```

## Arguments

- x:

  String or numeric. Rounded number. `x` must be a string unless
  `digits` is specified (most likely by a function that uses `unround()`
  as a helper).

- rounding:

  String. Rounding method presumably used to create `x`. Default is
  `"up_or_down"`. For more, see section `Rounding`.

- threshold:

  Integer. Number from which to round up or down. Other rounding methods
  are not affected. Default is `5`.

- digits:

  Integer. This argument is meant to make `unround()` more efficient to
  use as a helper function so that it doesn't need to redundantly count
  decimal places. Don't specify it otherwise. Default is `NULL`, in
  which case decimal places really are counted internally and `x` must
  be a string.

## Value

A tibble with seven columns: `range`, `rounding`, `lower`, `incl_lower`,
`x`, `incl_upper`, and `upper`. The `range` column is a handy
representation of the information stored in the columns from `lower` to
`upper`, in the same order.

## Details

The function is vectorized over `x` and `rounding`. This can be useful
to unround multiple numbers at once, or to check how a single number is
unrounded with different assumed rounding methods.

If both vectors have a length greater than 1, it must be the same
length. However, this will pair numbers with rounding methods, which can
be confusing. It is recommended that at least one of these input vectors
has length 1.

Why does `x` need to be a string if `digits` is not specified? In that
case, `unround()` must count decimal places by itself. If `x` then was
numeric, it wouldn't have any trailing zeros because these get dropped
from numerics.

Trailing zeros are as important for reconstructing boundary values as
any other trailing digits would be. Strings don't drop trailing zeros,
so they are used instead.

## Rounding

Depending on how `x` was rounded, the boundary values can be inclusive
or exclusive. The `incl_lower` and `incl_upper` columns in the resulting
tibble are `TRUE` in the first case and `FALSE` in the second. The
`range` column reflects this with equation and inequation signs.

However, these ranges are based on assumptions about the way `x` was
rounded. Set `rounding` to the rounding method that hypothetically lead
to `x`:

|                               |                         |
|-------------------------------|-------------------------|
| **Value of `rounding`**       | **Corresponding range** |
| `"up_or_down"` (default)      | `lower <= x <= upper`   |
| `"up"`                        | `lower <= x < upper`    |
| `"down"`                      | `lower < x <= upper`    |
| `"even"`                      | (no fix range)          |
| `"ceiling"`                   | `lower < x = upper`     |
| `"floor"`                     | `lower = x < upper`     |
| `"trunc"` (positive `x`)      | `lower = x < upper`     |
| `"trunc"` (negative `x`)      | `lower < x = upper`     |
| `"trunc"` (zero `x`)          | `lower < x < upper`     |
| `"anti_trunc"` (positive `x`) | `lower < x = upper`     |
| `"anti_trunc"` (negative `x`) | `lower = x < upper`     |
| `"anti_trunc"` (zero `x`)     | (undefined; `NA`)       |

Base R's own [`round()`](https://rdrr.io/r/base/Round.html) (R version
\>= 4.0.0), referenced by `rounding = "even"`, is reconstructed in the
same way as `"up_or_down"`, but whether the boundary values are
inclusive or not is hard to predict. Therefore, `unround()` checks if
they are, and informs you about it.

## See also

For more about rounding `"up"`, `"down"`, or to `"even"`, see
[`round_up()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md).

For more about the less likely `rounding` methods, `"ceiling"`,
`"floor"`, `"trunc"`, and `"anti_trunc"`, see
[`round_ceiling()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md).

## Examples

``` r
# By default, the function assumes that `x`
# was either rounded up or down:
unround(x = "2.7")
#> # A tibble: 1 × 7
#>   range                  rounding   lower incl_lower x     incl_upper upper
#>   <chr>                  <chr>      <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) <= 2.75 up_or_down  2.65 TRUE       2.7   TRUE        2.75

# If `x` was rounded up, run this:
unround(x = "2.7", rounding = "up")
#> # A tibble: 1 × 7
#>   range                 rounding lower incl_lower x     incl_upper upper
#>   <chr>                 <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 <= x(2.7) < 2.75 up        2.65 TRUE       2.7   FALSE       2.75

# Likewise with rounding down...
unround(x = "2.7", rounding = "down")
#> # A tibble: 1 × 7
#>   range                 rounding lower incl_lower x     incl_upper upper
#>   <chr>                 <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 < x(2.7) <= 2.75 down      2.65 FALSE      2.7   TRUE        2.75

# ...and with `base::round()` which, broadly
# speaking, rounds to the nearest even number:
unround(x = "2.7", rounding = "even")
#> # A tibble: 1 × 7
#>   range                rounding lower incl_lower x     incl_upper upper
#>   <chr>                <chr>    <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 2.65 < x(2.7) < 2.75 even      2.65 FALSE      2.7   FALSE       2.75

# Multiple input number-strings return
# multiple rows in the output data frame:
unround(x = c(3.6, "5.20", 5.174))
#> # A tibble: 3 × 7
#>   range                        rounding  lower incl_lower x     incl_upper upper
#>   <chr>                        <chr>     <dbl> <lgl>      <chr> <lgl>      <dbl>
#> 1 3.55 <= x(3.6) <= 3.65       up_or_do…  3.55 TRUE       3.6   TRUE        3.65
#> 2 5.195 <= x(5.20) <= 5.205    up_or_do…  5.20 TRUE       5.20  TRUE        5.20
#> 3 5.1735 <= x(5.174) <= 5.1745 up_or_do…  5.17 TRUE       5.174 TRUE        5.17
```
