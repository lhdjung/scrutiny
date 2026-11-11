# Is a vector a certain kind of sequence?

**\[deprecated\]**

These functions are deprecated because scrutiny no longer uses them: the
sequence mappers record how far each value was moved, in `diff_var`.
They may be removed in a future version.

Predicates for numeric vectors, or vectors coercible to numeric:

- `is_seq_linear()`: do all consecutive elements differ by the same
  amount?

- `is_seq_ascending()`, `is_seq_descending()`: is every difference
  positive, or negative?

- `is_seq_dispersed()`: is `x` centered on `from`, with each pair of
  values equidistant from it?

By default (`test_linear = TRUE`), the last three also test for
linearity.

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

  Numeric, character, or factor vector. Strings and factor labels count
  as numbers as determined by
  [`is_numeric_like()`](https://lhdjung.github.io/scrutiny/reference/is_numeric_like.md);
  any that are not make the result `FALSE`. A logical vector is accepted
  only if all its values are `NA`. Any other type, such as a list or a
  data frame, is an error.

- tolerance:

  Numeric. Tolerance for comparing values. Default is about `1.5e-8`, as
  in [`dplyr::near()`](https://dplyr.tidyverse.org/reference/near.html).
  It is never finer than the rounding error of arithmetic on the values,
  so `0` allows that error only.

- test_linear:

  Logical. Should linearity be tested as well? Default is `TRUE`. Not in
  `is_seq_linear()`.

- from:

  Numeric, character, or factor, like `x`. Only in `is_seq_dispersed()`.
  The value `x` should be centered on.

## Value

Logical (length 1). If `x` contains `NA`:

- `NA` if some values substituted for the `NA`s would make `x` the kind
  of sequence in question and others would not.

- `FALSE` if none would, e.g., because `x` is too short for it (two
  values for `is_seq_ascending()` and `is_seq_descending()`, three for
  `is_seq_dispersed()`).

- `TRUE` if all would, as any one or two values are linear.

A missing `from` is handled the same way.

## See also

`validate::is_linear_sequence()`, which is more permissive with `NA` and
supports date-times.

## Examples

``` r
is_seq_linear(x = 3:7)
#> Warning: `is_seq_linear()` was deprecated in scrutiny 1.0.0.
#> ℹ It may be removed in a future version of scrutiny.
#> [1] TRUE
is_seq_linear(x = c(3:7, 9))
#> [1] FALSE

# The other predicates test for linearity by default:
is_seq_ascending(x = c(2, 7, 9))
#> Warning: `is_seq_ascending()` was deprecated in scrutiny 1.0.0.
#> ℹ It may be removed in a future version of scrutiny.
#> [1] FALSE
is_seq_ascending(x = c(2, 7, 9), test_linear = FALSE)
#> [1] TRUE

is_seq_descending(x = c(9, 7, 2))
#> Warning: `is_seq_descending()` was deprecated in scrutiny 1.0.0.
#> ℹ It may be removed in a future version of scrutiny.
#> [1] FALSE
is_seq_descending(x = c(9, 7, 2), test_linear = FALSE)
#> [1] TRUE

is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5)
#> Warning: `is_seq_dispersed()` was deprecated in scrutiny 1.0.0.
#> ℹ It may be removed in a future version of scrutiny.
#> [1] FALSE
is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5, test_linear = FALSE)
#> [1] TRUE

# These fail their own test regardless of linearity:
is_seq_ascending(x = c(1, 7, 4), test_linear = FALSE)
#> [1] FALSE
is_seq_descending(x = c(9, 15, 3), test_linear = FALSE)
#> [1] FALSE
is_seq_dispersed(1:10, from = 5, test_linear = FALSE)
#> [1] FALSE
```
