# The DEBIT (descriptive binary) test

`debit()` checks whether the reported mean and standard deviation of
binary data are consistent with the reported sample size. With `k` ones
among `n` values, the mean is `k / n` and the SD follows from `k` and
`n`; the test asks whether any `k` produces both reported numbers at
their decimal places.

The function is vectorized, but
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
is more convenient for testing many cases.

## Usage

``` r
debit(
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

  String. `"exact"` (the default) only considers means that `n` binary
  values can have, i.e., `k / n` for a whole number `k`. `"mean_n"`
  treats the mean as continuous, as in Heathers and Brown (2019), and
  accepts some SDs that no binary sample of size `n` can have:
  `debit(0.05, 0.21, 20, 2, 2, "integer", formula = "mean_n")` is
  `TRUE`, although a mean of 0.05 with `n = 20` means one 1 in 20, whose
  SD rounds to 0.22.

- rounding:

  String. The rounding method assumed to have produced `x` and `sd`.
  Default is `"up_or_down"`. See
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) for
  the others.

- threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

Logical. `TRUE` if `x`, `sd`, and `n` are consistent, `FALSE` if not,
and `NA` if the case cannot be decided: if `n` is not a positive whole
number, or if a missing value could decide the case either way. See
[`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md),
section *Missing values*, on which cases a missing value leaves open. A
sample SD divides by `n - 1`, so under `sd_meaning = "sd_sample"` (the
default) and `"se"`, `n` must also be at least 2.

## Details

`x`, `sd`, `n`, `digits_x`, `digits_sd`, and `data_type` are vectorized
and recycled to a common length by the tidyverse rules. All other
arguments must have length 1.

`x` and `sd` are limited to about 15 significant digits, and `x` with
`n` as in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). Within
these limits, `debit()` decides value sets of any size exactly. With `x`
missing, `n` must be below 2^52, about 4.5e15.

## References

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## See also

[`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
to explain a result in plain words,
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
for data frames, and
[`debit_values()`](https://lhdjung.github.io/scrutiny/reference/debit_values.md)
and
[`debit_plot_line()`](https://lhdjung.github.io/scrutiny/reference/debit_plot_line.md)
for the means and SDs behind a value set.

## Examples

``` r
debit(
  x = 0.36,
  sd = 0.11,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> [1] FALSE
```
