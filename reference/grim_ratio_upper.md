# Upper bound for the GRIM ratio

**\[deprecated\]**

`grim_ratio_upper()` is deprecated because it no longer seems very
meaningful. It will be removed in a future version.

See
[`grim_probability()`](https://lhdjung.github.io/scrutiny/reference/grim-stats.md)
for a more interesting measure.

## Usage

``` r
grim_ratio_upper(x, percent = FALSE)
```

## Arguments

- x:

  String (length 1). Mean or percentage value computed from data with
  integer units, e.g., mean scores on a Likert scale or percentage of
  study participants in some condition. It has to be string to capture
  any trailing zeros.

- percent:

  Logical. Set `percent` to `TRUE` if `x` is expressed as a proportion
  of 100 rather than 1. The functions will then account for this fact
  through increasing the decimal count by 2. Default is `FALSE`.

## Value

Numeric.
