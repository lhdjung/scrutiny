# Count decimal places

`decimal_places()` counts the decimal places in a numeric vector, or in
a string vector that can be coerced to numeric.

`decimal_places_scalar()` is much faster but only takes a single input.
It is useful as a helper within other single-case functions.

## Usage

``` r
decimal_places(x, sep = "\\.")

decimal_places_scalar(x, sep = "\\.")
```

## Arguments

- x:

  Numeric (or string that can be coerced to numeric). Object with
  decimal places to count.

- sep:

  Substring that separates the mantissa from the integer part. Default
  is `"\\."`, which renders a decimal point.

## Value

Integer. Number of decimal places in `x`.

## Details

Decimal places in numeric values can't be counted accurately if the
number has 15 or more characters in total, including the integer part
and the decimal point. A possible solutions is to enter the number as a
string to count all digits. (Converting to string is not sufficient –
those numbers need to be *entered* in quotes.)

The functions ignore any whitespace at the end of a string, so they
won't mistake spaces for decimal places.

## Trailing zeros

If trailing zeros matter, don't convert numeric values to strings: In
numeric values, any trailing zeros have already been dropped, and any
information about them was lost (e.g., `3.70` returns `3.7`). Enter
those values as strings instead, such as `"3.70"` instead of `3.70`.
However, you can restore lost trailing zeros with
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
if the original number of decimal places is known.

If you need to enter many such values as strings, consider using
[`tibble::tribble()`](https://tibble.tidyverse.org/reference/tribble.html)
and drawing quotation marks around all values in a `tribble()` column at
once via RStudio's multiple cursors.

## See also

[`decimal_places_df()`](https://lhdjung.github.io/scrutiny/reference/decimal_places_df.md),
which applies `decimal_places()` to all numeric-like columns in a data
frame.

## Examples

``` r
# `decimal_places()` works on both numeric values
# and strings...
decimal_places(x = 2.851)
#> [1] 3
decimal_places(x = "2.851")
#> [1] 3

# ... but trailing zeros are only counted within
# strings:
decimal_places(x = c(7.3900, "7.3900"))
#> [1] 2 4

# This doesn't apply to non-trailing zeros; these
# behave just like any other digit would:
decimal_places(x = c(4.08, "4.08"))
#> [1] 2 2

# Whitespace at the end of a string is not counted:
decimal_places(x = "6.0     ")
#> [1] 1

# `decimal_places_scalar()` is much faster,
# but only works with a single number or string:
decimal_places_scalar(x = 8.13)
#> [1] 2
decimal_places_scalar(x = "5.024")
#> [1] 3
```
