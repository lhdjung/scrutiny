# Is a reported percentage consistent with its count?

**\[experimental\]**

`percentage_of()` checks whether `percentage` is what `absolute` out of
`n` would have been reported as, given the number of decimal places and
the rounding method.

This is stricter than
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) with
`x_meaning = "percentage"`, which only asks whether some count out of
`n` produces the percentage. Here, the count is reported too.

The function is vectorized, but
[`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
is more convenient for testing many cases.

## Usage

``` r
percentage_of(
  absolute,
  percentage,
  n,
  digits_percentage,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- absolute:

  Integer. The reported count.

- percentage:

  Numeric. The reported percentage of `n` that `absolute` is said to be,
  e.g., `33.3`.

- n:

  Integer. The reported total.

- digits_percentage:

  Integer. The number of decimal places in `percentage`, including
  trailing zeros. There is no default, since a numeric value does not
  keep them (`1.40` is `1.4`).

- rounding:

  String. The rounding method assumed to have produced `percentage`.
  Default is `"up_or_down"`, which accepts either direction. See
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) for
  the other methods.

- threshold:

  Numeric. For `rounding = "up_from"`, `"down_from"`, or
  `"up_from_or_down_from"`, the point within a step at which rounding
  switches direction, in tenths of a step. Default is `5`. Ignored
  otherwise.

- symmetric:

  Logical. If `TRUE`, negative numbers are rounded like their absolute
  values (`-2.5` like `2.5`) under `"up"`, `"down"`, and the `"*_from"`
  methods. Default is `FALSE`. Must not be `TRUE` with a `"ties_*"`
  rounding method, which already fixes the direction; see
  [`roundwork::reround()`](https://lhdjung.github.io/roundwork/reference/reround.html).

## Value

Logical. `TRUE` if the three values are consistent, `FALSE` if not, and
`NA` if the case cannot be decided: if a value is infinite, if
`absolute` is not a non-negative whole number, if `n` is not a positive
whole number, or if a missing value could decide the case either way. A
missing `absolute` gives `FALSE` if no count out of `n` has the
percentage, and a missing `n` gives `FALSE` if no `n` makes `absolute`
that percentage. A missing `digits_percentage` is treated as in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Details

The test runs in exact integer arithmetic. `absolute`, `percentage`,
`n`, and `digits_percentage` are vectorized; all other arguments must
have length 1.

An `absolute` above `n` is tested like any other, as for answers to a
question that allows several, whose percentages can sum to more than
100.

## Causes of inconsistencies

A `FALSE` result means that the three values cannot all be correct as
reported, but not why. The cause is often trivial: the percentage may be
based on another denominator than `n`, such as the number of valid
responses, or on an unconventional rounding method.
[`percentage_of_map_seq()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map_seq.md)
with `var = "n"` shows which nearby denominators would make the
percentage consistent.

## See also

[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) with
`x_meaning = "percentage"`, for a percentage without a count.

## Examples

``` r
# 40 out of 120 is 33.33...%, so it can be
# reported as 33 or 33.3, but not as 34:
percentage_of(40, 33, 120, digits_percentage = 0)
#> [1] TRUE
percentage_of(40, 33.3, 120, digits_percentage = 1)
#> [1] TRUE
percentage_of(40, 34, 120, digits_percentage = 0)
#> [1] FALSE
```
