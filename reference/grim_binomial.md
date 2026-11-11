# Poisson binomial test for the rate of GRIM consistency

**\[experimental\]**

Given the output of
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
`grim_binomial()` tests whether the rate of GRIM-consistent value sets
is above what random decimal digits would produce. This approach has not
been tested extensively; use it with care.

## Usage

``` r
grim_binomial(
  data,
  alternative = c("greater", "less", "two.sided"),
  alpha = 0.05
)
```

## Arguments

- data:

  Data frame returned by
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
  The output of
  [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
  or
  [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
  is rejected: its rows are hypothetical value sets, not reported ones.

- alternative:

  String (length 1). One of `"greater"` (the default), `"less"`, and
  `"two.sided"`.

- alpha:

  Numeric (length 1). Significance level of the test. The interval
  around `estimate` has a confidence level of `1 - alpha`. Default is
  `0.05`.

## Value

Tibble with the columns `estimate` (observed consistency rate),
`statistic` (number of consistent cases), `p.value`, `parameter` (number
of cases), `conf.low`, `conf.high`, `conf.level` (`1 - alpha`),
`method`, and `alternative`.

## Details

Under the null hypothesis, the decimal digits of each mean are random,
so its probability of consistency is `1 - exp_incon`, with `exp_incon`
from
[`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md).
Since this differs between value sets, the test uses the Poisson
binomial distribution rather than averaging the probabilities. Value
sets whose `consistency` or `exp_incon` is `NA` are left out;
`exp_incon` can be `NA` even where the value set has a definite result
(see
[`grim_exp_incon()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)).
Value sets with `data_type = "continuous"` have an `exp_incon` of 0, so
they count as certain successes: they leave the p-value unchanged but
raise `estimate` and the interval. Filter them out first to compare only
the others.

The two-sided p-value is twice the smaller one-sided one, capped at 1.

Correct value sets are always consistent, so a collection of them should
be more consistent than random digits even if some are wrong. There is
no corresponding reason to expect fewer consistent sets than random,
hence the one-sided default.

The confidence interval is the exact Clopper-Pearson interval for the
consistency rate, as in
[`stats::binom.test()`](https://rdrr.io/r/stats/binom.test.html), with
the same `alternative` as the test. It treats the value sets as sharing
one probability, so it is an interval for their average. Compare it to
the mean of `1 - exp_incon`.

## Examples

``` r
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  grim_binomial()
#> # A tibble: 1 × 9
#>   estimate statistic p.value parameter conf.low conf.high conf.level method     
#>      <dbl>     <int>   <dbl>     <int>    <dbl>     <dbl>      <dbl> <chr>      
#> 1    0.333         4   0.453        12    0.123         1       0.95 Poisson bi…
#> # ℹ 1 more variable: alternative <chr>
```
