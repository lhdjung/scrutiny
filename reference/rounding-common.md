# Common rounding procedures

`round_up()` rounds up from 5, `round_down()` rounds down from 5.
Otherwise, both functions work like
[`base::round()`](https://rdrr.io/r/base/Round.html).

`round_up()` and `round_down()` are special cases of `round_up_from()`
and `round_down_from()`, which allow users to choose custom thresholds
for rounding up or down, respectively.

## Usage

``` r
round_up_from(x, digits = 0L, threshold, symmetric = FALSE)

round_down_from(x, digits = 0L, threshold, symmetric = FALSE)

round_up(x, digits = 0L, symmetric = FALSE)

round_down(x, digits = 0L, symmetric = FALSE)
```

## Arguments

- x:

  Numeric. The decimal number to round.

- digits:

  Integer. Number of digits to round `x` to. Default is `0`.

- threshold:

  Integer. Only in `round_up_from()` and `round_down_from()`. Threshold
  for rounding up or down, respectively. Value is `5` in `round_up()`'s
  internal call to `round_up_from()` and in `round_down()`'s internal
  call to `round_down_from()`.

- symmetric:

  Logical. Set `symmetric` to `TRUE` if the rounding of negative numbers
  should mirror that of positive numbers so that their absolute values
  are equal. Default is `FALSE`.

## Value

Numeric. `x` rounded to `digits`.

## Details

These functions differ from
[`base::round()`](https://rdrr.io/r/base/Round.html) mainly insofar as
the decision about rounding 5 up or down is not based on the integer
portion of `x` (i.e., no "rounding to even"). Instead, in
`round_up_from()`, that decision is determined by the `threshold`
argument for rounding up, and likewise with `round_down_from()`. The
threshold is constant at `5` for `round_up()` and `round_down()`.

As a result, these functions are more predictable and less prone to
floating-point number quirks than
[`base::round()`](https://rdrr.io/r/base/Round.html). Compare
`round_down()` and [`base::round()`](https://rdrr.io/r/base/Round.html)
in the data frame for rounding 5 created in the Examples section below:
`round_down()` yields a continuous sequence of final digits from 0 to 9,
whereas [`base::round()`](https://rdrr.io/r/base/Round.html) behaves in
a way that can only be explained by floating point issues.

However, this surprising behavior on the part of
[`base::round()`](https://rdrr.io/r/base/Round.html) is not necessarily
a flaw (see its documentation, or this vignette:
https://rpubs.com/maechler/Rounding). In the present version of R (4.0.0
or later), [`base::round()`](https://rdrr.io/r/base/Round.html) works
fine, and the functions presented here are not meant to replace it.
Their main purpose as helpers within scrutiny is to reconstruct the
computations of researchers who might have used different software. See
[`vignette("rounding-options")`](https://lhdjung.github.io/scrutiny/articles/rounding-options.md).

## See also

[`round_ceiling()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
always rounds up,
[`round_floor()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
always rounds down,
[`round_trunc()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
always rounds toward 0, and
[`round_anti_trunc()`](https://lhdjung.github.io/scrutiny/reference/rounding-uncommon.md)
always round away from 0.

## Examples

``` r
# Both `round_up()` and `round_down()` work like
# `base::round()` unless the closest digit to be
# cut off by rounding is 5:

   round_up(x = 9.273, digits = 1)     # 7 cut off
#> [1] 9.3
 round_down(x = 9.273, digits = 1)     # 7 cut off
#> [1] 9.3
base::round(x = 9.273, digits = 1)     # 7 cut off
#> [1] 9.3

   round_up(x = 7.584, digits = 2)     # 4 cut off
#> [1] 7.58
 round_down(x = 7.584, digits = 2)     # 4 cut off
#> [1] 7.58
base::round(x = 7.584, digits = 2)     # 4 cut off
#> [1] 7.58


# Here is the borderline case of 5 rounded by
# `round_up()`, `round_down()`, and `base::round()`:

original <- c(    # Define example values
  0.05, 0.15, 0.25, 0.35, 0.45,
  0.55, 0.65, 0.75, 0.85, 0.95
)
tibble::tibble(   # Output table
  original,
  round_up = round_up(x = original, digits = 1),
  round_down = round_down(x = original, digits = 1),
  base_round = base::round(x = original, digits = 1)
)
#> # A tibble: 10 × 4
#>    original round_up round_down base_round
#>       <dbl>    <dbl>      <dbl>      <dbl>
#>  1     0.05      0.1        0          0  
#>  2     0.15      0.2        0.1        0.1
#>  3     0.25      0.3        0.2        0.2
#>  4     0.35      0.4        0.3        0.3
#>  5     0.45      0.5        0.4        0.4
#>  6     0.55      0.6        0.5        0.6
#>  7     0.65      0.7        0.6        0.7
#>  8     0.75      0.8        0.7        0.8
#>  9     0.85      0.9        0.8        0.8
#> 10     0.95      1          0.9        0.9

# (Note: Defining `original` as `seq(0.05:0.95, by = 0.1)`
# would lead to wrong results unless `original` is rounded
# to 2 or so digits before it's rounded to 1.)
```
