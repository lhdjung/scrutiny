# Power of the GRIM binomial test

**\[experimental\]**

`grim_binomial_power()` computes the statistical power of
[`grim_binomial()`](https://lhdjung.github.io/scrutiny/reference/grim_binomial.md)
for given parameters, to judge whether the test is informative for a
data set before interpreting its p-value. Like the test itself, it has
not been tested extensively.

## Usage

``` r
grim_binomial_power(
  p0,
  p1,
  k = NULL,
  alternative = c("greater", "less", "two.sided"),
  alpha = 0.05
)
```

## Arguments

- p0:

  Numeric. Null probability of GRIM consistency per value set, i.e.,
  `1 - grim_exp_incon(...)`. Either one value per value set, or a single
  value shared by all, in which case `k` is required.

- p1:

  Numeric (length 1). Probability of consistency under the alternative,
  shared by all value sets.

- k:

  Numeric (length 1). Number of value sets. Required if `p0` has length
  1; otherwise ignored in favor of `length(p0)`.

- alternative:

  String (length 1). One of `"greater"` (the default), `"less"`, and
  `"two.sided"`.

- alpha:

  Numeric (length 1). Significance level. Default is `0.05`.

## Value

Numeric (length 1). The probability of rejecting the null hypothesis
when the alternative is true.

## Examples

``` r
# 10 means with a null probability of 0.5 each, 90% truly consistent:
grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 10)
#> [1] 0.7360989

# The same with 30 means:
grim_binomial_power(p0 = 0.50, p1 = 0.9, k = 30)
#> [1] 0.9999109

# One null probability per value set:
grim_binomial_power(p0 = c(0.5, 0.7, 0.8, 0.9, 0.6, 0.5, 0.4), p1 = 0.9)
#> [1] 0.4782969
```
