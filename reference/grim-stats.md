# Possible GRIM inconsistencies

These functions compute statistics related to GRIM-testing. In general,
`grim_probability()` is the most useful of them, and it is responsible
for the `probability` column in a data frame returned by
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- `grim_probability()` returns the probability that a reported mean or
  percentage of integer data that is random except for the number of its
  decimal places is inconsistent with the reported sample size. For
  example, the mean 1.23 is treated like any other mean with two decimal
  places.

- `grim_ratio()` is equal to `grim_probability()` unless `grim_ratio()`
  is negative, which can occur if the sample size is very large.
  Strictly speaking, this is more informative than `grim_probability()`,
  but it is harder to interpret.

- `grim_total()` returns the absolute number of GRIM-inconsistencies
  that are possible given the mean or percentage's number of decimal
  places and the corresponding sample size.

For discussion, see
[`vignette("grim")`](https://lhdjung.github.io/scrutiny/articles/grim.md),
section *GRIM statistics*.

## Usage

``` r
grim_probability(x, n, items = 1, percent = FALSE)

grim_ratio(x, n, items = 1, percent = FALSE)

grim_total(x, n, items = 1, percent = FALSE)
```

## Arguments

- x:

  String (length 1). Mean or percentage value computed from data with
  integer units, e.g., mean scores on a Likert scale or percentage of
  study participants in some condition. It has to be string to capture
  any trailing zeros.

- n:

  Integer. Sample size corresponding to `x`.

- items:

  Integer. Number of items composing the mean or percentage value in
  question. Default is `1`.

- percent:

  Logical. Set `percent` to `TRUE` if `x` is expressed as a proportion
  of 100 rather than 1. The functions will then account for this fact
  through increasing the decimal count by 2. Default is `FALSE`.

## Value

Integer or double. The number of possible GRIM inconsistencies, or their
probability for a random mean or percentage with a given number of
decimal places.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## See also

[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) for the
GRIM test itself; as well as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
for applying it to many cases at once.

## Examples

``` r
# Many value sets are inconsistent here:
grim_probability(x = "83.29", n = 21)
#> [1] 0.79
grim_total(x = "83.29", n = 21)
#> [1] 79

# No sets are inconsistent in this case...
grim_probability(x = "5.14", n = 83)
#> [1] 0.17
grim_total(x = "5.14", n = 83)
#> [1] 17

# ... but most would be if `x` was a percentage:
grim_probability(x = "5.14", n = 83, percent = TRUE)
#> [1] 0.9917
grim_total(x = "5.14", n = 83, percent = TRUE)
#> [1] 9917
```
