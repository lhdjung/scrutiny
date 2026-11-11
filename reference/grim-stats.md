# Possible GRIM inconsistencies

These functions quantify how informative a GRIM test is for a given
sample size and number of decimal places:

- `grim_exp_incon()` returns the probability that a mean or percentage
  with `digits_x` random decimal digits is GRIM-inconsistent with `n`.
  It is the `exp_incon` column of
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- `grim_total()` returns the number of such inconsistent values between
  two whole numbers, out of `10^digits_x` (`10^(digits_x + 2)` with
  `x_meaning = "percentage"`).

See
[`vignette("grim-in-depth")`](https://lhdjung.github.io/scrutiny/articles/grim-in-depth.md),
section *GRIM statistics*.

## Usage

``` r
grim_exp_incon(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  percent = deprecated()
)

grim_total(
  x,
  n,
  digits_x,
  items_averaged,
  data_type,
  x_meaning = c("mean", "percentage"),
  rounding = "up_or_down",
  threshold = 5,
  percent = deprecated()
)
```

## Arguments

- x:

  Numeric. The mean or percentage. Only whether it is missing or
  infinite matters; the argument mirrors
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- n:

  Integer. The sample size.

- digits_x:

  Integer. The number of decimal places in `x`, including trailing
  zeros. There is no default, since a numeric value does not keep them
  (`1.40` is `1.4`).

- items_averaged:

  Integer. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- data_type:

  String. As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). No
  default. For `"continuous"` data, which GRIM can't find inconsistent,
  the result is 0, or `NA` where
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) is
  `NA`.

- x_meaning:

  String. `"mean"` (the default) or `"percentage"`, which adds 2 to the
  decimal count, as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- rounding, threshold:

  As in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md). The
  count depends on the method; `"even"` counts a tie on either bound as
  consistent, as
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) does.
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  passes its own on.

- percent:

  **\[deprecated\]** Replaced by `x_meaning`: use
  `x_meaning = "percentage"` instead of `percent = TRUE`.

## Value

Double. For integer data, `grim_exp_incon()` and `grim_total()` return
`NA` if `x` is missing or infinite, if `digits_x` is missing, or if `n`
or `items_averaged` is missing or not a positive whole number. These
include every case that
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) cannot
decide, and some that it can: a mean may pass whatever `n` is, but how
many fail depends on it. For continuous data, they return 0, or `NA`
where [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md)
is `NA`. Under `data_type = NA`, they return `NA` unless the count is 0.
A count above 2^53, which only `digits_x` of 14 or more with
`x_meaning = "percentage"` can reach, is the nearest number a double can
hold.

## Details

`x`, `n`, `digits_x`, `items_averaged`, and `data_type` are recycled to
a common length by the tidyverse rules: an argument has length 1 or the
length of the longest.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) and
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

## Examples

``` r
# Many value sets are inconsistent here:
grim_exp_incon(
  x = 83.29,
  n = 21,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] 0.79
grim_total(
  x = 83.29,
  n = 21,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] 79

# None are in this case...
grim_exp_incon(
  x = 5.14,
  n = 120,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] 0
grim_total(
  x = 5.14,
  n = 120,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] 0

# ...but most would be if `x` were a percentage:
grim_exp_incon(
  x = 5.14,
  n = 120,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer",
  x_meaning = "percentage"
)
#> [1] 0.988
grim_total(
  x = 5.14,
  n = 120,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer",
  x_meaning = "percentage"
)
#> [1] 9880
```
