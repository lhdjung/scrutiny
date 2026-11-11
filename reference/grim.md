# The GRIM test (granularity-related inconsistency of means)

`grim()` checks whether a reported mean of integer data is consistent
with the reported sample size and the number of items that compose the
mean. The mean of `n * items_averaged` integers is a whole number
divided by `n * items_averaged`, so only certain values are possible;
the test asks whether any of them would have been reported as `x`.

The function is vectorized, but
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
is more convenient for testing many cases.

## Usage

``` r
grim(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  percent = deprecated()
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

  Integer. The number of items averaged into each respondent's score: 1
  for a single item or the percentage of participants with some answer,
  4 for the mean of a four-item scale, 10 for the average percent
  correct on a ten-question test. Not the number of scale points. No
  default.

- data_type:

  String. The kind of data that `x` summarizes. No default.

  - `"integer"`: whole numbers, such as answers on a rating scale,
    counts, or yes/no answers for a percentage.

  - `"continuous"`: anything else, such as body mass index or a visual
    analogue scale. The test does not apply, so the result is `TRUE`, or
    `NA` for a value set that describes no data, such as one with
    `n = 0`.

  If `data_type` is `NA`, the result is `NA` unless both types give the
  same one.

- x_meaning:

  String. What `x` is: `"mean"` (the default) or `"percentage"`. A
  percentage is divided by 100 and tested with two more decimal places,
  as the proportion it is. See *Defaults* in `grim()`.

- rounding:

  String. The rounding method assumed to have produced `x`. Default is
  `"up_or_down"`, which accepts either direction; see *Defaults* in
  `grim()`. The others are `"up_from_or_down_from"`,
  `"ceiling_or_floor"`, `"up"`, `"down"`, `"even"`, `"up_from"`,
  `"down_from"`, `"ceiling"`, `"floor"`, `"trunc"`, `"anti_trunc"`,
  `"ties_up"`, `"ties_down"`, `"ties_away"`, `"ties_zero"`, and
  `"ties_even"`. See
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

- percent:

  **\[deprecated\]** Replaced by `x_meaning`: use
  `x_meaning = "percentage"` instead of `percent = TRUE`.

## Value

Logical. `TRUE` if `x`, `n`, and `items_averaged` are consistent,
`FALSE` if not, and `NA` if the case cannot be decided: if a value is
infinite, if `n` or `items_averaged` is not a positive whole number, or
if a missing value could decide the case either way. See
[`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md),
section *Missing values*, on which cases a missing value leaves open.

## Details

`items_averaged` must be specified because a mean over several items is
a mean of more values, which changes the test. A default of 1 would let
multi-item scales pass unnoticed. `data_type` must be specified because
GRIM only applies to whole numbers. A default of `"integer"` would let
the test flag means of continuous data, which can take any value.

`x`, `n`, `digits_x`, `items_averaged`, and `data_type` are vectorized
and recycled to a common length by the tidyverse rules: an argument has
length 1 or the length of the longest. All other arguments must have
length 1.

Numbers beyond the precision of a double are an error rather than a
guess. `x` with its `digits_x` may have about 15 significant digits:
`x * 10^(digits_x + 1)` must be below 2^52, about 4.5e15, and
`(x * 10^(digits_x + 1) + 10) * n * items_averaged` below 2^53, about
9e15. A `threshold` with decimal places raises the power of ten.

## Defaults

The defaults are meant to flag a value set only if it is inconsistent
under every usual convention:

- `rounding = "up_or_down"`: Reports rarely say how they rounded, and
  the common methods only disagree on ties. SPSS and Excel round a 5
  away from zero, while R's
  [`round()`](https://rdrr.io/r/base/Round.html) and Python's
  [`round()`](https://rdrr.io/r/base/Round.html) round it to an even
  digit. The default accepts a tie rounded either way, so a value set
  that fails is inconsistent under all of them. Choose another method
  only if you know which one was used. `threshold` and `symmetric` do
  not affect it.

- `x_meaning = "mean"`: GRIM was devised for means, and most values that
  it is applied to are means. A percentage needs to be marked as one,
  since the same number, such as 71, can be either.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md)
to explain a result in plain words,
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
for data frames,
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
for the means that `n` integers can produce, and
[`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
for how informative the test is in a given case.

## Examples

``` r
# A mean of 5.19 is not consistent with a sample size of 28:
grim(x = 5.19, n = 28, digits_x = 2, items_averaged = 1, data_type = "integer")
#> [1] FALSE

# However, it is consistent with a sample size of 32:
grim(x = 5.19, n = 32, digits_x = 2, items_averaged = 1, data_type = "integer")
#> [1] TRUE

# For a scale composed of two items:
grim(x = 2.84, n = 16, digits_x = 2, items_averaged = 2, data_type = "integer")
#> [1] TRUE

# With percentages instead of means -- here, 71%:
grim(
  x = 71,
  n = 43,
  digits_x = 0,
  items_averaged = 1,
  data_type = "integer",
  x_meaning = "percentage"
)
#> [1] FALSE

# Means of continuous data, such as reaction times, can take any value:
grim(
  x = 5.19,
  n = 28,
  digits_x = 2,
  items_averaged = 1,
  data_type = "continuous"
)
#> [1] TRUE
```
