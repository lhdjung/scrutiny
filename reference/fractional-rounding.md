# Generalized rounding to the nearest fraction of a specified denominator

Two functions that round numbers to specific fractions, not just to the
next higher decimal level. They are inspired by
`janitor::round_to_fraction()` but feature all the options of
[`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md):

- `reround_to_fraction()` closely follows `janitor::round_to_fraction()`
  by first rounding to fractions of a whole number, then optionally
  rounding the result to a specific number of digits in the usual way.

- `reround_to_fraction_level()` rounds to the nearest fraction of a
  number at the specific decimal level (i.e., number of digits), without
  subsequent rounding. This is closer to conventional rounding
  functions.

## Usage

``` r
reround_to_fraction(
  x = NULL,
  denominator = 1,
  digits = Inf,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)

reround_to_fraction_level(
  x = NULL,
  denominator = 1,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. Vector of numbers to be rounded.

- denominator:

  Numeric (\>= 1) . `x` will be rounded to the nearest fraction of
  `denominator`. Default is `1`.

- digits:

  Numeric (whole numbers).

  - In `reround_to_fraction()`: If `digits` is specified, the values
    resulting from fractional rounding will subsequently be rounded to
    that many decimal places. If set to `"auto"`, it internally becomes
    `ceiling(log10(denominator)) + 1`, as in
    `janitor::round_to_fraction()`. Default is `Inf`, in which case
    there is no subsequent rounding.

  - In `reround_to_fraction_level()`: This function will round to a
    fraction of the number at the decimal level specified by `digits`.
    Default is `0`.

- rounding, threshold, symmetric:

  More arguments passed down to
  [`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md).

## Value

Numeric vector of the same length as `x` unless `rounding` is either of
`"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`. In
these cases, it will always have length 2.

## See also

[`reround()`](https://lhdjung.github.io/scrutiny/reference/reround.md),
which the functions wrap, and `janitor::round_to_fraction()`, part of
which they copy.

## Examples

``` r
#`reround_to_fraction()` rounds `0.4`
# to `0` if `denominator` is `1`, which
# is the usual integer rounding...
reround_to_fraction(0.4, denominator = 1, rounding = "even")
#> [1] 0

# ...but if `denominator` is `2`, it rounds to the nearest
# fraction of 2, which is `0.5`:
reround_to_fraction(0.4, denominator = 2, rounding = "even")
#> [1] 0.5

# Likewise with fractions of 3:
reround_to_fraction(0.25, denominator = 3, rounding = "even")
#> [1] 0.3333333

# The default for `rounding` is to round
# both up and down, as in `reround()`:
reround_to_fraction(0.4, denominator = 2)
#> [1] 0.5 0.5

# These two rounding procedures differ
# at the tie points:
reround_to_fraction(0.25, denominator = 2)
#> [1] 0.5 0.0

# `reround_to_fraction_level()`, in contrast,
# uses `digits` to determine some decimal level,
# and then rounds to the closest fraction at
# that level:
reround_to_fraction_level(0.12345, denominator = 2, digits = 0)
#> [1] 0 0
reround_to_fraction_level(0.12345, denominator = 2, digits = 1)
#> [1] 0.1 0.1
reround_to_fraction_level(0.12345, denominator = 2, digits = 2)
#> [1] 0.125 0.125
```
