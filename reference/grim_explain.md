# Explain a GRIM test step by step

**\[experimental\]**

`grim_explain()` tests a single mean like
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) does,
but also lays out the calculation in plain words: which true means the
reported one could stand for, which sums of the underlying integers
follow from them, and whether any of these sums is possible.

The text is built from the numbers that decide the test itself, so it
always agrees with
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Usage

``` r
grim_explain(
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
  as the proportion it is. See *Defaults* in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

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

An object of class `scrutiny_explain`: a list with the result as
`consistency` (`TRUE`, `FALSE`, or `NA`), the explanation's
`paragraphs`, and the `test` and its `inputs`. It prints as the
explanation.

## See also

[`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md)
and
[`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
for the other tests, and
[`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
for mapper output.

## Examples

``` r
grim_explain(
  x = 5.19,
  n = 28,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> GRIM test: x = 5.19, n = 28, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 5.19, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 5.185 to 5.195.
#> 
#> The mean of 28 integers is their sum divided by 28. So the sum must be from
#> 5.185 * 28 = 145.18 to 5.195 * 28 = 145.46.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 145 and 146. They give 145 / 28 = 5.1786 and
#> 146 / 28 = 5.2143, which round to 5.18 and 5.21, not to 5.19.
#> 
#> ==> Inconsistent

grim_explain(
  x = 5.19,
  n = 32,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> GRIM test: x = 5.19, n = 32, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 5.19, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 5.185 to 5.195.
#> 
#> The mean of 32 integers is their sum divided by 32. So the sum must be from
#> 5.185 * 32 = 165.92 to 5.195 * 32 = 166.24.
#> 
#> A sum of integers is itself an integer, and 166 is in this range:
#> 166 / 32 = 5.1875, which rounds to 5.19.
#> 
#> ==> Consistent

grim_explain(
  x = 71,
  n = 43,
  digits_x = 0,
  items_averaged = 1,
  data_type = "integer",
  x_meaning = "percentage"
)
#> GRIM test: x = 71, n = 43, items_averaged = 1, data_type = "integer", x_meaning = "percentage"
#> 
#> The percentage was reported as 71%, with no decimal places. Before rounding
#> (assuming either rounding up from 5 or down from 5), it could have been
#> anything from 70.5% to 71.5%.
#> 
#> A percentage out of 43 is a count divided by 43, times 100. So 71% stands for
#> the proportion 0.71, with two more decimal places than the percentage shows.
#> The count must be from 70.5 / 100 * 43 = 30.315 to 71.5 / 100 * 43 = 30.745.
#> 
#> A count is an integer, but there is no integer in this range. The nearest ones
#> are 30 and 31. They give 30 / 43 * 100 = 69.7674 and 31 / 43 * 100 = 72.093,
#> which round to 70% and 72%, not to 71%.
#> 
#> ==> Inconsistent
```
