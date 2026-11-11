# Achievable means and SDs of binary data

**\[experimental\]**

`debit_values()` returns every pair of a mean and an SD that `n` binary
values (0 and 1) can have and that would be reported as `x` and `sd`. It
is the counterpart of
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
and
[`grimmer_values()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md)
for [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
which is `TRUE` exactly if there is at least one such pair.

`debit_closest()` returns the single achievable pair whose SD is closest
to `sd`, like
[`grimmer_closest()`](https://lhdjung.github.io/scrutiny/reference/grimmer_values.md).
If `x` and `sd` are consistent, it is one of the pairs that
`debit_values()` returns. Otherwise, it is closest among the pairs whose
mean would be reported as `x`, or, if there are none, it is the pair
whose mean
[`grim_closest()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md)
returns.

## Usage

``` r
debit_values(
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

debit_closest(
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

  String. `"integer"` if the values are 0 and 1, as in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).
  There is no default, so that every call states this assumption.
  `"continuous"` is an error: such data can have any value that would be
  reported as the one given, so there are none to list. Under `NA`, a
  value set is undecidable unless no data can have it.

- sd_meaning:

  String. As in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  but `"sd_sample_or_pop"` is not allowed: the same data have a
  different SD under each premise, so call `debit_values()` once for
  each. Under `"se"`, the `sd` column holds the standard error.

- formula:

  String. As in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
  but only `"exact"`, the default, is allowed: `"mean_n"` takes the mean
  as anything that would be reported as `x`, so there are no values to
  list.

- rounding:

  String. The rounding method assumed to have produced `x` and `sd`.
  Default is `"up_or_down"`. See
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) for
  the others.

- threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

A tibble with one row per achievable pair. `case` is the index of the
value set, as in
[`grim_values()`](https://lhdjung.github.io/scrutiny/reference/grim_values.md).
`x` and `sd` are the achievable values, `n` the reported sample size,
and `sum` the number of ones, `k` in the mean `k / n`. An inconsistent
value set has no rows, and one that cannot be decided has one row of
`NA` but for `n`, as in
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).

`debit_closest()` returns the same columns with exactly one row per
value set: of `NA` but for `n` if a value is missing or the value set
cannot be decided.

## Details

More than a million numbers of ones whose mean would be reported as `x`
is an error, since they are listed before the SD narrows them down. That
takes an `n` of more than about `10^(digits_x + 6)`.

In `debit_closest()`, pairs whose SDs are equally close go by the mean
closer to `x`, then by the smaller number of ones.

## See also

[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md),
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
[`debit_plot_line()`](https://lhdjung.github.io/scrutiny/reference/debit_plot_line.md),
and
[`sd_binary_groups()`](https://lhdjung.github.io/scrutiny/reference/sd-binary.md).

## Examples

``` r
# A mean of 0.35 and an SD of 0.49 are consistent with 20 binary values,
# 7 of them ones:
debit_values(
  x = 0.35,
  sd = 0.49,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> # A tibble: 1 × 5
#>    case     x    sd     n   sum
#>   <int> <dbl> <dbl> <int> <dbl>
#> 1     1  0.35 0.489    20     7

# None if the value set is inconsistent:
debit_values(
  x = 0.35,
  sd = 0.41,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> # A tibble: 0 × 5
#> # ℹ 5 variables: case <int>, x <dbl>, sd <dbl>, n <int>, sum <dbl>

# With few decimal places and a large sample, there can be several:
debit_values(
  x = 0.5,
  sd = 0.5,
  n = 300,
  digits_x = 1,
  digits_sd = 1,
  data_type = "integer"
)
#> # A tibble: 31 × 5
#>     case     x    sd     n   sum
#>    <int> <dbl> <dbl> <int> <dbl>
#>  1     1 0.45  0.498   300   135
#>  2     1 0.453 0.499   300   136
#>  3     1 0.457 0.499   300   137
#>  4     1 0.46  0.499   300   138
#>  5     1 0.463 0.499   300   139
#>  6     1 0.467 0.500   300   140
#>  7     1 0.47  0.500   300   141
#>  8     1 0.473 0.500   300   142
#>  9     1 0.477 0.500   300   143
#> 10     1 0.48  0.500   300   144
#> # ℹ 21 more rows

# The closest pair to an inconsistent one:
debit_closest(
  x = 0.35,
  sd = 0.41,
  n = 20,
  digits_x = 2,
  digits_sd = 2,
  data_type = "integer"
)
#> # A tibble: 1 × 5
#>    case     x    sd     n   sum
#>   <int> <dbl> <dbl> <int> <dbl>
#> 1     1  0.35 0.489    20     7
```
