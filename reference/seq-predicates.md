# Is a vector a certain kind of sequence?

Predicate functions that test whether `x` is a numeric vector (or
coercible to numeric) with some special properties:

- `is_seq_linear()` tests whether every two consecutive elements of `x`
  differ by some constant amount.

- `is_seq_ascending()` and `is_seq_descending()` test whether the
  difference between every two consecutive values is positive or
  negative, respectively. `is_seq_dispersed()` tests whether `x` values
  are grouped around a specific central value, `from`, with the same
  distance to both sides per value pair. By default
  (`test_linear = TRUE`), these functions also test for linearity, like
  `is_seq_linear()`.

`NA` elements of `x` are handled in a nuanced way. See *Value* section
below and the examples in
[`vignette("devtools")`](https://lhdjung.github.io/scrutiny/articles/devtools.md),
section *NA handling*.

## Usage

``` r
is_seq_linear(x, tolerance = .Machine$double.eps^0.5)

is_seq_ascending(x, test_linear = TRUE, tolerance = .Machine$double.eps^0.5)

is_seq_descending(x, test_linear = TRUE, tolerance = .Machine$double.eps^0.5)

is_seq_dispersed(
  x,
  from,
  test_linear = TRUE,
  tolerance = .Machine$double.eps^0.5
)
```

## Arguments

- x:

  Numeric or coercible to numeric, as determined by
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md).
  Vector to be tested.

- tolerance:

  Numeric. Tolerance of comparison between numbers when testing. Default
  is circa 0.000000015 (1.490116e-08), as in
  [`dplyr::near()`](https://dplyr.tidyverse.org/reference/near.html).

- test_linear:

  Logical. In functions other than `is_seq_linear()`, should `x` also be
  tested for linearity? Default is `TRUE`.

- from:

  Numeric or coercible to numeric. Only in `is_seq_dispersed()`. It will
  test whether `from` is at the center of `x`, and if every pair of
  other values is equidistant to it.

## Value

A single logical value. If `x` contains at least one `NA` element, the
functions return either `NA` or `FALSE`:

- If all elements of `x` are `NA`, the functions return `NA`.

- If some but not all elements are `NA`, they check if `x` *might* be a
  sequence of the kind in question: Is it a linear (and / or ascending,
  etc.) sequence after the `NA`s were replaced by appropriate values? If
  so, they return `NA`; otherwise, they return `FALSE`.

## See also

`validate::is_linear_sequence()`, which is much like `is_seq_linear()`
but more permissive with `NA` values. It comes with some additional
features, such as support for date-times.

## Examples

``` r
# These are linear sequences...
is_seq_linear(x = 3:7)
#> [1] TRUE
is_seq_linear(x = c(3:7, 8))
#> [1] TRUE

# ...but these aren't:
is_seq_linear(x = c(3:7, 9))
#> [1] FALSE
is_seq_linear(x = c(10, 3:7))
#> [1] FALSE

# All other `is_seq_*()` functions
# also test for linearity by default:
is_seq_ascending(x = c(2, 7, 9))
#> [1] FALSE
is_seq_ascending(x = c(2, 7, 9), test_linear = FALSE)
#> [1] TRUE

is_seq_descending(x = c(9, 7, 2))
#> [1] FALSE
is_seq_descending(x = c(9, 7, 2), test_linear = FALSE)
#> [1] TRUE

is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5)
#> [1] FALSE
is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5, test_linear = FALSE)
#> [1] TRUE

# These fail their respective
# individual test even
# without linearity testing:
is_seq_ascending(x = c(1, 7, 4), test_linear = FALSE)
#> [1] FALSE
is_seq_descending(x = c(9, 15, 3), test_linear = FALSE)
#> [1] FALSE
is_seq_dispersed(1:10, from = 5, test_linear = FALSE)
#> [1] FALSE
```
