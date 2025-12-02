# Uncommon rounding procedures

Always round up, down, toward zero, or away from it:

- `round_ceiling()` always rounds up.

- `round_floor()` always rounds down.

- `round_trunc()` always rounds toward zero.

- `round_anti_trunc()` always rounds away from zero. (`0` itself is
  rounded to `1`.)

- `anti_trunc()` does not round but otherwise works like
  `round_anti_trunc()`.

Despite not being widely used, they are featured here in case they are
needed for reconstruction.

## Usage

``` r
round_ceiling(x, digits = 0L)

round_floor(x, digits = 0L)

round_trunc(x, digits = 0L)

anti_trunc(x)

round_anti_trunc(x, digits = 0L)
```

## Arguments

- x:

  Numeric. The decimal number to round.

- digits:

  Integer. Number of digits to round `x` to. Default is `0`.

## Value

Numeric. `x` rounded to `digits` (except for `anti_trunc()`, which has
no `digits` argument).

## Details

`round_ceiling()`, `round_floor()`, and `round_trunc()` generalize the
base R functions [`ceiling()`](https://rdrr.io/r/base/Round.html),
[`floor()`](https://rdrr.io/r/base/Round.html), and
[`trunc()`](https://rdrr.io/r/base/Round.html), and include them as
special cases: With the default value for `digits`, 0, these `round_*`
functions are equivalent to their respective base counterparts.

The last `round_*` function, `round_anti_trunc()`, generalizes another
function presented here: `anti_trunc()` works like
[`trunc()`](https://rdrr.io/r/base/Round.html) except it moves away from
0, rather than towards it. That is, whereas
[`trunc()`](https://rdrr.io/r/base/Round.html) minimizes the absolute
value of `x` (as compared to the other rounding functions),
`anti_trunc()` maximizes it. `anti_trunc(x)` is therefore equal to
`trunc(x)` ` + 1` if `x` is positive, and to `trunc(x) - 1` if `x` is
negative.

`round_anti_trunc()`, then, generalizes `anti_trunc()` just as
`round_ceiling()` generalizes
[`ceiling()`](https://rdrr.io/r/base/Round.html), etc.

Moreover, `round_trunc()` is equivalent to `round_floor()` for positive
numbers and to `round_ceiling()` for negative numbers. The reverse is
again true for `round_anti_trunc()`: It is equivalent to
`round_ceiling()` for positive numbers and to `round_floor()` for
negative numbers.

## See also

[`round_up()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
and
[`round_down()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
round up or down from 5, respectively.
[`round_up_from()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
and
[`round_down_from()`](https://lhdjung.github.io/scrutiny/reference/rounding-common.md)
allow users to specify custom thresholds for rounding up or down.

## Examples

``` r
# Always round up:
round_ceiling(x = 4.52, digits = 1)        # 2 cut off
#> [1] 4.6

# Always round down:
round_floor(x = 4.67, digits = 1)          # 7 cut off
#> [1] 4.6

# Always round toward 0:
round_trunc(8.439, digits = 2)             # 9 cut off
#> [1] 8.43
round_trunc(-8.439, digits = 2)            # 9 cut off
#> [1] -8.43

# Always round away from 0:
round_anti_trunc(x = 8.421, digits = 2)    # 1 cut off
#> [1] 8.43
round_anti_trunc(x = -8.421, digits = 2)   # 1 cut off
#> [1] -8.43
```
