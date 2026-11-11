# Granularity of non-continuous scales

`grim_granularity()` returns the smallest difference between two means
that a scale with `items_averaged` items and `n` observations can
produce: `1 / (n * items_averaged)`.

## Usage

``` r
grim_granularity(n, items_averaged = 1)
```

## Arguments

- n:

  Numeric. Sample size.

- items_averaged:

  Numeric. Number of items averaged into each score. Default is 1.

## Value

Numeric. Granularity.

## Details

The granularity of a scale follows from its design, not from the
smallest observed difference between two values. Continuous scales have
no granularity.

## References

Brown, N. J. L., & Heathers, J. A. J. (2017). The GRIM Test: A Simple
Technique Detects Numerous Anomalies in the Reporting of Results in
Psychology. *Social Psychological and Personality Science*, 8(4),
363–369. https://journals.sagepub.com/doi/10.1177/1948550616673876

## Examples

``` r
grim_granularity(n = 16)
#> [1] 0.0625
grim_granularity(n = 16, items_averaged = 2)
#> [1] 0.03125
```
