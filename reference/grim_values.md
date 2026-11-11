# Achievable means behind a reported one

**\[experimental\]**

These functions return the means (or percentages) that a sample of
`n * items_averaged` integers can have:

- `grim_values()` returns every achievable value that would be reported
  as `x`, with the whole-number sum behind it. The result has no rows if
  `x` is GRIM-inconsistent with `n`.

- `grim_closest()` returns the single achievable value closest to `x`
  among those that would be reported as `x`, or, if there are none, the
  closest one overall. Of two equally close values, it returns the
  smaller.

Both are vectorized like
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md); `case`
says which value set a row belongs to.
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
and
[`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
are the counterparts for GRIMMER.

## Usage

``` r
grim_values(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)

grim_closest(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. The reported mean or percentage.

- n:

  Integer. The reported sample size.

- digits_x:

  Integer. The number of decimal places in `x`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`).

- items_averaged:

  Integer. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- data_type:

  String. `"integer"` if the values are whole numbers, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).
  There is no default, so that every call states this assumption.
  `"continuous"` is an error: such data can have any value that would be
  reported as the one given, so there are none to list. Under `NA`, a
  value set is undecidable unless no data can have it.

- x_meaning:

  String. `"mean"` (the default) or `"percentage"`, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- rounding, threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

- For `grim_values()`, a tibble with one row per achievable mean. `case`
  is the index of the value set, i.e., the position in the inputs after
  they are recycled to a common length, so it also counts through `n` if
  `x` is a single value. `x` is the achievable value, `n` the reported
  sample size, `items_averaged` as given, and `sum` the whole number
  behind `x`, the sum of all `n * items_averaged` item scores. An
  inconsistent value set has no rows, and one that cannot be decided has
  one row of `NA` but for `n` and `items_averaged`, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). So
  does one with a missing `digits_x` that is consistent whatever it is:
  how many values would be reported as `x` still depends on it.

- For `grim_closest()`, a tibble with the same columns and exactly one
  row per value set: the closest achievable mean, or a row of `NA` but
  for `n` and `items_averaged` if the value set cannot be decided.

## Details

The achievable means are the whole numbers in the range that
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
shows as `sum_lower` and `sum_upper` under `show_rec`, each divided by
`n * items_averaged`. Once `n * items_averaged` exceeds `10^digits_x`,
several of them can be reported as the same `x`, and their number grows
with `n * items_averaged`. GRIM is then no longer informative.
`grim_closest()` stays cheap in that case. `grim_values()` does not, and
more than a million values for one value set is an error.

With `x_meaning = "percentage"`, the values are percentages, like `x`.

Under `rounding = "even"`, or its synonym `"ties_even"`, both bounds of
that range are inclusive (see
[`roundwork::unround()`](https://lhdjung.github.io/roundwork/reference/unround.html)),
so the values include ties that
[`base::round()`](https://rdrr.io/r/base/Round.html) may round away from
`x`: `grim_values(1.3, 20, 1, 1, "integer", rounding = "even")` returns
1.25 and 1.35 along with 1.3.

With `items_averaged > 1`, the integers are the respondents' totals
across the items, so `sum` is the sum of the totals.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md),
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md),
and
[`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md).

## Examples

``` r
# A mean of 5.19 is consistent with a sample size of 32,
# and there is exactly one way to get there:
grim_values(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 1 × 5
#>    case     x     n items_averaged   sum
#>   <int> <dbl> <int>          <int> <dbl>
#> 1     1  5.19    32              1   166

# With a larger sample, several means round to 5.19:
grim_values(x = 5.19, n = 300, digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 3 × 5
#>    case     x     n items_averaged   sum
#>   <int> <dbl> <int>          <int> <dbl>
#> 1     1  5.19   300              1  1556
#> 2     1  5.19   300              1  1557
#> 3     1  5.19   300              1  1558

# It is not consistent with a sample size of 28...
grim_values(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 0 × 5
#> # ℹ 5 variables: case <int>, x <dbl>, n <int>, items_averaged <int>, sum <dbl>

# ...but 5.19 is close to a mean that is achievable:
grim_closest(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#> # A tibble: 1 × 5
#>    case     x     n items_averaged   sum
#>   <int> <dbl> <int>          <int> <dbl>
#> 1     1  5.18    28              1   145

# Both functions are vectorized. In `grim_values()`, `case` is the index of
# the value set, and an inconsistent one has no rows:
grim_values(
  x = c(5.19, 4.2),
  n = c(28, 30),
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 1 × 5
#>    case     x     n items_averaged   sum
#>   <int> <dbl> <int>          <int> <dbl>
#> 1     2   4.2    30              1   126
grim_closest(
  x = c(5.19, 4.2),
  n = c(28, 30),
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 2 × 5
#>    case     x     n items_averaged   sum
#>   <int> <dbl> <int>          <int> <dbl>
#> 1     1  5.18    28              1   145
#> 2     2  4.2     30              1   126
```
