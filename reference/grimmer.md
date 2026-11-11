# The GRIMMER test (granularity-related inconsistency of means mapped to error repeats)

`grimmer()` checks whether a reported mean and standard deviation of
integer data are consistent with the reported sample size and the number
of items averaged. It extends
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md): the
mean must pass GRIM, and the SD must then be reachable from one of the
integer sums that GRIM admits.

The function is vectorized, but
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
is more convenient for testing many cases.

## Usage

``` r
grimmer(
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
  `grimmer()`.

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

Logical. `TRUE` if `x`, `sd`, `n`, and `items_averaged` are consistent,
`FALSE` if not, and `NA` if the case cannot be decided: if a value is
infinite, if `n` or `items_averaged` is not a positive whole number, or
if a missing value could decide the case either way. See
[`vignette("technical-notes")`](https://lhdjung.github.io/scrutiny/articles/technical-notes.md),
section *Missing values*, on which cases a missing value leaves open. A
sample SD divides by `n - 1`, so under `sd_meaning = "sd_sample"` (the
default) and `"se"`, `n` must also be at least 2.

## Details

`x`, `sd`, `n`, `digits_x`, `digits_sd`, `items_averaged`, and
`data_type` are vectorized and recycled to a common length by the
tidyverse rules. All other arguments must have length 1.

The implementation follows Allard's (2018) Analytic-GRIMMER algorithm,
as does `rsprite2::GRIMMER_test()`, with a stricter first test: the sum
of squares must be one that `n` whole numbers with the reconstructed sum
can have. For `n >= 5`, this means at least the smallest such sum of
squares; for smaller `n`, some larger ones are impossible, too. With
this test, `grimmer()` is `TRUE` if and only if a sample of whole
numbers has a mean and SD that would be reported as `x` and `sd`. The
one exception is `rounding = "even"`, or its synonym `"ties_even"`,
which takes a value on a rounding bound to round either way, as in
[`roundwork::unround()`](https://lhdjung.github.io/roundwork/reference/unround.html),
so it may also accept a value set that a sample only rounds to if ties
are broken the other way. GRIMMER was first proposed by Anaya (2016).

For data on a bounded scale, such as a Likert scale, see the
[strait](https://github.com/ianhussey/strait) package. Its `brimmest()`
decides exactly whether a mean and SD are possible within the bounds.

`x` and `sd` are limited to about 15 significant digits, and `x` with
`n` and `items_averaged` as in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). Within
these limits, `grimmer()` decides value sets of any size exactly, but
refuses two kinds that no study reports: with `n` from 2 to 4, SDs above
about 3e5 to 1e6 (divided by `items_averaged`), depending on `n` and
`sd_meaning`; and value sets with more than a million candidate sums,
which takes an `n` of more than a million.

## Defaults

`sd_meaning = "sd_sample"` because the sample SD is what nearly all
software reports by default: R's
[`sd()`](https://rdrr.io/r/stats/sd.html), SPSS, Stata, SAS, Excel's
`STDEV()` and `STDEV.S()`, and pandas' `.std()`. The main exception is
NumPy, whose `np.std()` returns the population SD unless `ddof = 1` is
given; so does Python's `statistics.pstdev()`. If the data may have been
analyzed that way, or an inconsistent value set has a small `n`, test it
under `"sd_pop"` as well. See *SD meaning*.

The rounding defaults are explained in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## SD meaning

The sample and population SDs differ by a factor of `sqrt(n / (n - 1))`,
which matters most at small `n`, where GRIMMER is most powerful. A
population SD tested as a sample SD will often seem inconsistent, so
`sd_meaning` lets you check whether a value set is consistent only under
the other convention.

`"sd_sample_or_pop"` is `TRUE` if the value set is consistent as a
sample SD or as a population SD, and `FALSE` if it is neither. A single
value has no sample SD, so at `n = 1`, `"sd_sample_or_pop"` is the same
as `"sd_pop"`. The `reason` column of
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
names the convention a value set passed under, trying the sample SD
first.

`"sd_sample_or_pop"` accepts more SDs than either convention alone, so
it is not the default. It also judges each value set on its own,
although a single study usually computed all its SDs the same way. To
tell which convention fits a set of values, test them under
`"sd_sample"` and `"sd_pop"` separately.

Some papers report a standard error as if it were an SD. `"se"` tests
whether a value set is consistent under that premise. The SE was
rounded, not the SD, so the SD may lie anywhere in the SE's rounding
range times `sqrt(n)`. This range is wide: from about `n = 30` on,
nearly every value set whose mean passes GRIM is consistent under
`"se"`. Such a result says little, and it is no evidence that an SE was
reported.

`"se"` is the SE of this group's mean, computed from its own sample SD
and `n`. Model-based SEs, such as those of estimated marginal means, or
SEs based on a pooled variance, are not. A value set that is
inconsistent under `"se"` may still have been reported with one of them.

## References

Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
possibility of standard deviations.
https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/

Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
reported measures of variability. *PeerJ Preprints.*
https://peerj.com/preprints/2400v1/

## See also

[`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md)
to explain a result in plain words,
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md)
for data frames, and
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
for the means and SDs behind a consistent value set.

## Examples

``` r
# A mean of 5.23 is not consistent with an SD of 2.55
# and a sample size of 35:
grimmer(
  x = 5.23,
  sd = 2.55,
  n = 35,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] FALSE

# However, mean and SD are consistent with a
# sample size of 31:
grimmer(
  x = 5.23,
  sd = 2.55,
  n = 31,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] TRUE

# For a scale composed of two items:
grimmer(
  x = 2.74,
  sd = 0.96,
  n = 63,
  digits_x = 2,
  digits_sd = 2,
  items_averaged = 2,
  data_type = "integer"
)
#> [1] TRUE
```
