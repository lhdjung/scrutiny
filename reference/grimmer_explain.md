# Explain a GRIMMER test step by step

**\[experimental\]**

`grimmer_explain()` tests a single mean and SD like
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md)
does, but also lays out the calculation in plain words. It starts with
the GRIM test of the mean, as in
[`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md),
and goes on to the sum of squares that each possible sum implies with
the SD.

The text is built from the numbers that decide the test itself, so it
always agrees with
[`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md).

## Usage

``` r
grimmer_explain(
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

  String. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md):
  `"integer"` or `"continuous"`. No default. Continuous data can have
  any mean and any SD of 0 or more, but a single value only an SD of 0.

- sd_meaning:

  String. What `sd` really is. `"sd_sample"` (the default) is the sample
  SD, which divides by `n - 1`; `"sd_pop"` is the population SD, which
  divides by `n`; `"sd_sample_or_pop"` accepts either; `"se"` is the
  standard error of the mean, the sample SD divided by `sqrt(n)`,
  reported as if it were an SD. See *Defaults* and *SD meaning* in
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md).

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

## Examples

``` r
grimmer_explain(
  x = 5.23,
  sd = 2.55,
  n = 35,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> GRIMMER test: x = 5.23, sd = 2.55, n = 35, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 5.23, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 5.225 to 5.235.
#> 
#> The mean of 35 integers is their sum divided by 35. So the sum must be from
#> 5.225 * 35 = 182.875 to 5.235 * 35 = 183.225.
#> 
#> A sum of integers is itself an integer, and 183 is in this range:
#> 183 / 35 = 5.2286, which rounds to 5.23.
#> 
#> The SD was reported as 2.55, with 2 decimal places. Before rounding, it could
#> have been anything from 2.545 to 2.555.
#> 
#> Together, the sum and the SD determine the sum of squares, which is what you
#> get by squaring each of the 35 values and adding them up. It is
#> (35 - 1) * SD^2 + sum^2 / 35. The values are integers, so their squares are
#> integers, too, and so is the sum of squares. What's more, an integer and its
#> square are either both odd or both even. So the sum of squares must be odd if
#> the sum is odd, and even if the sum is even.
#> 
#> With a sum of 183, the sum of squares is from
#> 34 * 2.545^2 + 183^2 / 35 = 1177.0474 to 34 * 2.555^2 + 183^2 / 35 = 1178.7814.
#> The only integer in this range is 1178, which is even, but 183 is odd.
#> 
#> ==> Inconsistent

grimmer_explain(
  x = 5.23,
  sd = 2.55,
  n = 31,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> GRIMMER test: x = 5.23, sd = 2.55, n = 31, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 5.23, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 5.225 to 5.235.
#> 
#> The mean of 31 integers is their sum divided by 31. So the sum must be from
#> 5.225 * 31 = 161.975 to 5.235 * 31 = 162.285.
#> 
#> A sum of integers is itself an integer, and 162 is in this range:
#> 162 / 31 = 5.2258, which rounds to 5.23.
#> 
#> The SD was reported as 2.55, with 2 decimal places. Before rounding, it could
#> have been anything from 2.545 to 2.555.
#> 
#> Together, the sum and the SD determine the sum of squares, which is what you
#> get by squaring each of the 31 values and adding them up. It is
#> (31 - 1) * SD^2 + sum^2 / 31. The values are integers, so their squares are
#> integers, too, and so is the sum of squares. What's more, an integer and its
#> square are either both odd or both even. So the sum of squares must be odd if
#> the sum is odd, and even if the sum is even.
#> 
#> With a sum of 162, the sum of squares is from
#> 30 * 2.545^2 + 162^2 / 31 = 1040.8914 to 30 * 2.555^2 + 162^2 / 31 = 1042.4214.
#> The integers from 1041 to 1042 are in this range, so one of them is even, like
#> 162.
#> 
#> ==> Consistent
```
