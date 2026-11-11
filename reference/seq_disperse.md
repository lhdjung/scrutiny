# Sequences dispersed around a value

`seq_disperse()` creates a sequence of values around `from`, a given
number of steps up and down, where a step is one unit of the last
decimal place: `7.93` is surrounded by `7.91`, `7.92`, `7.94`, `7.95`,
and so on.

`seq_disperse_df()` returns the sequence as a data frame column, with
further columns added as in
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
Its arguments are those of `seq_disperse()` with a leading dot.

## Usage

``` r
seq_disperse(
  from,
  by = NULL,
  dispersion = 1:5,
  offset_from = 0L,
  out_min = "auto",
  out_max = NULL,
  include_reported = TRUE,
  track_diff_var = FALSE
)

seq_disperse_df(
  .from,
  .by = NULL,
  ...,
  .dispersion = 1:5,
  .offset_from = 0L,
  .out_min = "auto",
  .out_max = NULL,
  .include_reported = TRUE,
  .track_diff_var = FALSE
)
```

## Arguments

- from, .from:

  Numeric, or a string coercible to numeric. The value to disperse from.

- by, .by:

  Numeric. Step size. Default is `NULL`, in which case it is one unit of
  the last decimal place of `from`. A number does not keep trailing
  zeros, so `5.30` steps by `0.1`: give `by = 0.01`, or `from` as the
  string `"5.30"`.

- dispersion, .dispersion:

  Numeric. Whole numbers of steps up and down from `from`. Default is
  `1:5`.

- offset_from, .offset_from:

  Integer. Moves the starting point by that many steps. Default is `0`.

- out_min, .out_min, out_max, .out_max:

  Limits on the values, including `from` itself. `out_min` defaults to
  `"auto"`, one step above zero, which suits a count but not a quantity
  that can reach zero or be negative; use `NULL` for an unbounded
  quantity such as a mean, or `0` for one bounded at zero such as a
  standard deviation. `out_max` defaults to `NULL`, no maximum, and
  `"auto"` is the same.

- include_reported, .include_reported:

  Logical. Should `from` itself be part of the sequence? Default is
  `TRUE`.

- track_diff_var, .track_diff_var:

  Logical. Should the number of steps from `from` be returned as well?
  Default is `FALSE`. If `TRUE`, `seq_disperse()` returns a list of the
  sequence and the steps, and `seq_disperse_df()` adds a `diff_var`
  column.

- ...:

  Further columns, added as in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
  Only in `seq_disperse_df()`.

## Value

- `seq_disperse()` returns a numeric vector, which is integer if `from`
  is an integer and every value is a whole number. With
  `track_diff_var = TRUE`, it returns a list of that vector and an
  integer vector of steps.

- `seq_disperse_df()` returns a tibble with the sequence in the `x`
  column, the steps in `diff_var` if requested, and any columns added
  via the dots.

## Details

Unlike [`base::seq()`](https://rdrr.io/r/base/seq.html), these functions
need not return a regular sequence: `dispersion` can be any non-negative
whole numbers, in any order, such as `c(1, 4, 9)`. The values follow
that order, mirrored around `from`. Steps of 0 and repeated steps are
dropped.

## See also

[`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
and [`base::seq()`](https://rdrr.io/r/base/seq.html), whose ideas
`seq_disperse()` combines.

## Examples

``` r
seq_disperse(from = 4.02)
#>  [1] 3.97 3.98 3.99 4.00 4.01 4.02 4.03 4.04 4.05 4.06 4.07

# A number loses trailing zeros, so state the step size:
seq_disperse(from = 5.30, by = 0.01)
#>  [1] 5.25 5.26 5.27 5.28 5.29 5.30 5.31 5.32 5.33 5.34 5.35

# More steps:
seq_disperse(from = 4.02, dispersion = 1:10)
#>  [1] 3.92 3.93 3.94 3.95 3.96 3.97 3.98 3.99 4.00 4.01 4.02 4.03 4.04 4.05 4.06
#> [16] 4.07 4.08 4.09 4.10 4.11 4.12

# Irregular steps:
seq_disperse(from = 4.02, dispersion = c(2, 3, 7))
#> [1] 3.95 3.99 4.00 4.02 4.04 4.05 4.09

# The data frame variant takes further columns:
seq_disperse_df(.from = 4.02, n = 45)
#> # A tibble: 11 × 2
#>        x     n
#>    <dbl> <dbl>
#>  1  3.97    45
#>  2  3.98    45
#>  3  3.99    45
#>  4  4       45
#>  5  4.01    45
#>  6  4.02    45
#>  7  4.03    45
#>  8  4.04    45
#>  9  4.05    45
#> 10  4.06    45
#> 11  4.07    45
```
