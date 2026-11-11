# Standard deviation of binary data

Compute the SD of binary data (0 and 1 values) from different inputs:

- `sd_binary_groups()` from the counts of 0s and 1s.

- `sd_binary_0_n()` from the count of 0s and the total.

- `sd_binary_1_n()` from the count of 1s and the total.

- `sd_binary_mean_n()` from the mean and the total.

These are the SDs that
[`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md) tests
under the same `sd_meaning`.

**\[superseded\]** `sd_binary_0_n()` and `sd_binary_1_n()` are
superseded by `sd_binary_groups()`: `sd_binary_0_n(group_0, n)` is
`sd_binary_groups(group_0, n - group_0)`.

## Usage

``` r
sd_binary_groups(
  group_0,
  group_1,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
)

sd_binary_0_n(
  group_0,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
)

sd_binary_1_n(
  group_1,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
)

sd_binary_mean_n(
  x,
  n,
  sd_meaning = c("sd_sample", "sd_pop", "sd_sample_or_pop", "se"),
  mean = deprecated()
)
```

## Arguments

- group_0:

  Integer. Count of 0s.

- group_1:

  Integer. Count of 1s.

- sd_meaning:

  String. What to compute: the sample SD, `"sd_sample"` (the default);
  the population SD, `"sd_pop"`; or the standard error of the mean,
  `"se"`, as in
  [`debit()`](https://lhdjung.github.io/scrutiny/reference/debit.md).
  `"sd_sample_or_pop"` is an error here, since it stands for two
  premises with different values.

- n:

  Integer. Total sample size.

- x:

  Numeric. Mean of the data, i.e., the share of 1s.

- mean:

  **\[deprecated\]** The old name of `x`.

## Value

Numeric. The SD or standard error; `NA` where `n` or a count is not a
whole number, and where `n` is below 2 under `"sd_sample"` and `"se"`,
which divide by `n - 1`.

## References

Heathers, James A. J., and Brown, Nicholas J. L. 2019. DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## Examples

``` r
# 127 values are 0 and 153 are 1, so n = 280:
sd_binary_groups(group_0 = 127, group_1 = 153)
#> [1] 0.4987311
sd_binary_0_n(group_0 = 127, n = 280)
#> [1] 0.4987311
sd_binary_1_n(group_1 = 153, n = 280)
#> [1] 0.4987311

# From the mean, given enough decimal places:
sd_binary_mean_n(x = 0.5464286, n = 280)
#> [1] 0.4987311

# The population SD and the standard error:
sd_binary_groups(group_0 = 127, group_1 = 153, sd_meaning = "sd_pop")
#> [1] 0.4978397
sd_binary_groups(group_0 = 127, group_1 = 153, sd_meaning = "se")
#> [1] 0.02980488
```
