# Compute rounding bias

Rounding often leads to bias, such that the mean of a rounded
distribution is different from the mean of the original distribution.
Call `rounding_bias()` to compute the amount of this bias.

## Usage

``` r
rounding_bias(
  x,
  digits,
  rounding = "up",
  threshold = 5,
  symmetric = FALSE,
  mean = TRUE
)
```

## Arguments

- x:

  Numeric or string coercible to numeric.

- digits:

  Integer. Number of decimal digits to which `x` will be rounded.

- rounding:

  String. Rounding procedure that will be applied to `x`. See
  [`vignette("rounding-options")`](https://lhdjung.github.io/scrutiny/articles/rounding-options.md).
  Default is `"up"`.

- threshold, symmetric:

  Further arguments passed down to
  [`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md).

- mean:

  Logical. If `TRUE` (the default), the mean total of bias will be
  returned. Set `mean` to `FALSE` to get a vector of individual biases
  the length of `x`.

## Value

Numeric. By default of `mean`, the length is 1; otherwise, it is the
same length as `x`.

## Details

Bias is calculated by subtracting the original vector, `x`, from a
vector rounded in the specified way.

The function passes all arguments except for `mean` down to
[`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md).
Other than there, however, `rounding` is `"up"` by default, and it can't
be set to `"up_or_down"`, `"up_from_or_down_from"`,
or`"ceiling_or_floor"`.

## Examples

``` r
# Define example vector:
vec <- seq_distance(0.01, string_output = FALSE)
vec
#>  [1] 0.01 0.02 0.03 0.04 0.05 0.06 0.07 0.08 0.09 0.10

# The default rounds `x` up from 5:
rounding_bias(x = vec, digits = 1)
#> [1] 0.005

# Other rounding procedures are supported,
# such as rounding down from 5...
rounding_bias(x = vec, digits = 1, rounding = "down")
#> [1] -0.005

# ...or rounding to even with `base::round()`:
rounding_bias(x = vec, digits = 1, rounding = "even")
#> [1] -0.005
```
