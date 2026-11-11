# Achievable means and SDs behind a reported pair

**\[experimental\]**

`grimmer_values()` returns every pair of a mean and an SD that a sample
of `n` integers can have and that would be reported as `x` and `sd`. It
is the counterpart of
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
for
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
which is `TRUE` exactly if there is at least one such pair.

`grimmer_closest()` returns the single achievable pair whose SD is
closest to `sd`, like
[`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
for means. If `x` and `sd` are consistent, it is one of the pairs that
`grimmer_values()` returns. Otherwise, it is closest among the pairs
whose mean would be reported as `x`, or, if there are none, whose mean
is the one
[`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
returns.

Each pair comes with the whole-number sum and sum of squares behind it.
Statistics computed from several groups, such as a t-value, can be
compared exactly through these integers.

## Usage

``` r
grimmer_values(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)

grimmer_closest(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  items_averaged,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. The reported mean.

- sd:

  Numeric. The reported standard deviation.

- n:

  Integer. The reported sample size.

- digits_x:

  Integer. The number of decimal places in `x`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`).

- digits_sd:

  Integer. The number of decimal places in `sd`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`).

- items_averaged:

  Integer. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), for
  `x` and `sd`.

- data_type:

  String. `"integer"` if the values are whole numbers, as in
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md).
  There is no default, so that every call states this assumption.
  `"continuous"` is an error: such data can have any value that would be
  reported as the one given, so there are none to list. Under `NA`, a
  value set is undecidable unless no data can have it.

- sd_meaning:

  String. As in
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  but `"sd_sample_or_pop"` is not allowed: the same sum of squares gives
  a different SD under each premise, so call `grimmer_values()` once for
  each. Under `"se"`, the `sd` column holds the standard error.

- rounding:

  String. The rounding method assumed to have produced `x`. Default is
  `"up_or_down"`, which accepts either direction; see *Defaults* in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). The
  others are `"up_from_or_down_from"`, `"ceiling_or_floor"`, `"up"`,
  `"down"`, `"even"`, `"up_from"`, `"down_from"`, `"ceiling"`,
  `"floor"`, `"trunc"`, `"anti_trunc"`, `"ties_up"`, `"ties_down"`,
  `"ties_away"`, `"ties_zero"`, and `"ties_even"`. See
  [`vignette("rounding-options", package = "roundwork")`](https://lhdjung.github.io/roundwork/articles/rounding-options.html).

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

A tibble with one row per achievable pair. `case` is the index of the
value set, as in
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md).
`x` and `sd` are the achievable values, `n` the reported sample size,
`items_averaged` as given, and `sum` and `sum_squares` the whole numbers
behind them. An inconsistent value set has no rows, and one that cannot
be decided has one row of `NA` but for `n` and `items_averaged`, as in
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md).

`grimmer_closest()` returns the same columns with exactly one row per
value set: of `NA` but for `n` and `items_averaged` if a value is
missing or the value set cannot be decided.

## Details

The sums are the whole numbers that
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
divides by `n * items_averaged`. For each, the sums of squares are the
whole numbers within the rounding bounds of `sd` that `n` integers with
that sum can have. With `items_averaged > 1`, the integers are the
respondents' totals across the items, so the sums are those of the
totals.

The number of pairs grows with `n`, `items_averaged`, and `sd`, and
shrinks with `digits_x` and `digits_sd`. More than a million pairs for
one value set is an error, since the output has one row for each. So is
a sum of squares of 2^53 or more, about 9e15, which a double can't hold
exactly;
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
tests such value sets all the same. `grimmer_closest()` stops at 2^48,
about 3e14, beyond which it can miss the closest pair. With `n` from 2
to 4, both stop at 2^40, about 1.1e12, as
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
does; `grimmer_closest()` stops there even if `x` fails GRIM.

Under `rounding = "even"`, the pairs include ties that
[`base::round()`](https://rdrr.io/r/base/Round.html) may round away from
`x` or `sd`, as in
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md).

In `grimmer_closest()`, pairs whose SDs are equally close go by the mean
closer to `x`, then by the smaller sum, then by the smaller SD. A
negative `sd` is closest to the least SD there is. The number of pairs
doesn't matter there, but that of the sums does: for a consistent value
set, it tries one sum per remainder modulo `n`, and more than 100,000 of
them is an error. That takes an `n` of more than about
`10^(digits_x + 5)`.

## See also

[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md),
and
[`grimmer_plot_line()`](https://lhdjung.github.io/scrutiny/reference/grimmer_plot_line.md).

## Examples

``` r
# A mean of 5.23 and an SD of 2.55 are consistent with a sample size of 31,
# and there is exactly one way to get there:
grimmer_values(
  x = 5.23,
  sd = 2.55,
  n = 31,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 1 × 7
#>    case     x    sd     n items_averaged   sum sum_squares
#>   <int> <dbl> <dbl> <int>          <int> <dbl>       <dbl>
#> 1     1  5.23  2.55    31              1   162        1042

# With one decimal place, several pairs round to the same values:
grimmer_values(
  x = 5.3,
  sd = 0.7,
  n = 30,
  digits_x = 1,
  digits_sd = 1,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 7 × 7
#>    case     x    sd     n items_averaged   sum sum_squares
#>   <int> <dbl> <dbl> <int>          <int> <dbl>       <dbl>
#> 1     1  5.27 0.691    30              1   158         846
#> 2     1  5.27 0.740    30              1   158         848
#> 3     1  5.3  0.651    30              1   159         855
#> 4     1  5.3  0.702    30              1   159         857
#> 5     1  5.3  0.750    30              1   159         859
#> 6     1  5.33 0.661    30              1   160         866
#> 7     1  5.33 0.711    30              1   160         868

# None at all if the value set is inconsistent:
grimmer_values(
  x = 5.23,
  sd = 2.55,
  n = 35,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 0 × 7
#> # ℹ 7 variables: case <int>, x <dbl>, sd <dbl>, n <int>, items_averaged <int>,
#> #   sum <dbl>, sum_squares <dbl>

# ...but there is a pair close to it:
grimmer_closest(
  x = 5.23,
  sd = 2.55,
  n = 35,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> # A tibble: 1 × 7
#>    case     x    sd     n items_averaged   sum sum_squares
#>   <int> <dbl> <dbl> <int>          <int> <dbl>       <dbl>
#> 1     1  5.23  2.54    35              1   183        1177
```
