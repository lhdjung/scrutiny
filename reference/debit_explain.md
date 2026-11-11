# Explain a DEBIT test step by step

**\[experimental\]**

`debit_explain()` tests a single mean and SD of binary data like
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) does,
but also lays out the calculation in plain words: which numbers of ones
the mean allows, and whether the SD of any of them would be reported as
the given one.

The text is built from the numbers that decide the test itself, so it
always agrees with
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).

## Usage

``` r
debit_explain(
  x,
  sd,
  n,
  digits_x,
  digits_sd,
  data_type,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  formula = c("exact", "mean_n"),
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- x:

  Numeric. The reported mean of the binary data, between 0 and 1.

- sd:

  Numeric. The reported standard deviation, between 0 and 1.

- n:

  Integer. The reported sample size.

- digits_x, digits_sd:

  Integer. The number of decimal places in `x` and `sd`, including
  trailing zeros. There are no defaults, since a numeric value does not
  keep them (`1.40` is `1.4`).

- data_type:

  String. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md):
  `"integer"` or `"continuous"`. No default. For DEBIT, `"integer"`
  means the binary values 0 and 1 that the test assumes. Continuous data
  can have any mean and SD from 0 to 1, so the result is `TRUE`, but a
  single value can only have an SD of 0. Values outside of 0 to 1 are an
  error for either data type.

- sd_meaning:

  String. What `sd` really is: the sample SD, `"sd_sample"` (the
  default); the population SD, `"sd_pop"`; either one,
  `"sd_sample_or_pop"`; or the standard error of the mean, `"se"`. As in
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md),
  whose sections *Defaults* and *SD meaning* say why the sample SD is
  the default and when to choose another.

- formula:

  String. Only `"exact"`, the default, can be explained.

- rounding:

  String. The rounding method assumed to have produced `x` and `sd`.
  Default is `"up_or_down"`. See
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) for
  the others.

- threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

An object of class `scrutiny_explain`: a list with the result as
`consistency` (`TRUE`, `FALSE`, or `NA`), the explanation's
`paragraphs`, and the `test` and its `inputs`. It prints as the
explanation.

## Examples

``` r
debit_explain(
  x = 0.36,
  sd = 0.11,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> DEBIT test: x = 0.36, sd = 0.11, n = 20, data_type = "integer"
#> 
#> DEBIT is about binary data, where every value is 0 or 1. The mean is then the
#> share of ones, and the SD follows from the number of ones and the sample size.
#> 
#> The mean was reported as 0.36, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 0.355 to 0.365.
#> 
#> With 20 values, the mean is the number of ones divided by 20, so that number
#> must be from 0.355 * 20 = 7.1 to 0.365 * 20 = 7.3. But it has to be an integer,
#> and there is none in this range. So no number of ones fits the mean, whatever
#> the SD is.
#> 
#> ==> Inconsistent

debit_explain(
  x = 0.35,
  sd = 0.49,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> DEBIT test: x = 0.35, sd = 0.49, n = 20, data_type = "integer"
#> 
#> DEBIT is about binary data, where every value is 0 or 1. The mean is then the
#> share of ones, and the SD follows from the number of ones and the sample size.
#> 
#> The mean was reported as 0.35, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 0.345 to 0.355.
#> 
#> With 20 values, the mean is the number of ones divided by 20, so that number
#> must be from 0.345 * 20 = 6.9 to 0.355 * 20 = 7.1. It has to be an integer, so
#> it can only be 7.
#> 
#> The SD was reported as 0.49, with 2 decimal places. Before rounding, it could
#> have been anything from 0.485 to 0.495.
#> 
#> With k ones among 20 values, the SD is sqrt(k * (20 - k) / (20 * 19)). For
#> k = 7, that is sqrt(7 * 13 / 380) = 0.4894, which is in that range.
#> 
#> ==> Consistent
```
