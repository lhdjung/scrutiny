# Number of items from granularity

**\[deprecated\]**

`grim_items()` is deprecated and will be removed in a future version of
scrutiny. Use `1 / (n * gran)` instead.

## Usage

``` r
grim_items(n, gran, tolerance = .Machine$double.eps^0.5)
```

## Arguments

- n:

  Numeric. Sample size.

- gran:

  Numeric. Granularity.

- tolerance:

  Numeric. Differences from a whole number below `tolerance` are ignored
  when checking the result. Default is about `1.5e-8`.
