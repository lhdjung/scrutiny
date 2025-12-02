# Sequence generation at decimal level

Functions that provide a smooth interface to generating sequences based
on the input values' decimal depth. Each function creates a sequence
with a step size of one unit on the level of the input values' ultimate
decimal digit (e.g., `2.45`, `2.46`, `2.47`, ...):

- `seq_endpoint()` creates a sequence from one input value to another.
  For step size, it goes by the value with more decimal places.

- `seq_distance()` only takes the starting point and, instead of the
  endpoint, the desired output length. For step size, it goes by the
  starting point by default.

`seq_endpoint_df()` and `seq_distance_df()` are variants that create a
data frame. Further columns can be added as in
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
Regular arguments are the same as in the respective non-`df` function,
but with a dot before each.

## Usage

``` r
seq_endpoint(from, to, offset_from = 0L, offset_to = 0L, string_output = TRUE)

seq_distance(
  from,
  by = NULL,
  length_out = 10L,
  dir = 1,
  offset_from = 0L,
  string_output = TRUE
)

seq_endpoint_df(
  .from,
  .to,
  ...,
  .offset_from = 0L,
  .offset_to = 0L,
  .string_output = TRUE
)

seq_distance_df(
  .from,
  .by = NULL,
  ...,
  .length_out = 10L,
  .dir = 1,
  .offset_from = 0L,
  .string_output = TRUE
)
```

## Arguments

- from, .from:

  Numeric (or string coercible to numeric). Starting point of the
  sequence.

- to, .to:

  Numeric (or string coercible to numeric). Endpoint of the sequence.
  Only in `seq_endpoint()` and `seq_endpoint_df()`.

- offset_from, .offset_from:

  Integer. If set to a non-zero number, the starting point will be
  offset by that many units on the level of the last decimal digit.
  Default is `0`.

- offset_to, .offset_to:

  Integer. If set to a non-zero number, the endpoint will be offset by
  that many units on the level of the last decimal digit. Default is
  `0`. Only in `seq_endpoint()` and `seq_endpoint_df()`.

- string_output, .string_output:

  Logical or string. If `TRUE` (the default), the output is a string
  vector. Decimal places are then padded with zeros to match `from`'s
  (or `to`'s) number of decimal places. `"auto"` works like `TRUE` if
  and only if `from` (`.from`) is a string.

- by, .by:

  Numeric. Only in `seq_distance()` and `seq_distance_df()`. Step size
  of the sequence. If not set, inferred automatically. Default is
  `NULL`.

- length_out, .length_out:

  Integer. Length of the output vector (i.e., the number of its values).
  Default is `10`. Only in `seq_distance()` and `seq_distance_df()`.

- dir, .dir:

  Integer. If set to `-1`, the sequence goes backward. Default is `1`.
  Only in `seq_distance()` and `seq_distance_df()`.

- ...:

  Further columns, added as in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
  Only in `seq_endpoint_df()` and `seq_distance_df()`.

## Value

String by default of `string_output`, numeric otherwise.

## Details

If either `from` or `to` ends on zero, be sure to enter that value as a
string! This is crucial because trailing zeros get dropped from numeric
values. A handy way to format numeric values or number-strings correctly
is
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md).
The output of the present functions is like that by default (of
`string_output`).

In `seq_endpoint()` and `seq_endpoint_df()`, the step size is determined
by `from` and `to`, whichever has more decimal places. In
`seq_distance()` and `seq_distance_df()`, it's determined by the decimal
places of `from`.

These functions are scrutiny's take on
[`base::seq()`](https://rdrr.io/r/base/seq.html), and themselves
wrappers around it.

## See also

[`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
for sequences centered around the input.

## Examples

``` r
# Sequence between two points:
seq_endpoint(from = 4.7, to = 5)
#> [1] "4.7" "4.8" "4.9" "5.0"

# Sequence of some length; default is 10:
seq_distance(from = 0.93)
#>  [1] "0.93" "0.94" "0.95" "0.96" "0.97" "0.98" "0.99" "1.00" "1.01" "1.02"
seq_distance(from = 0.93, length_out = 5)
#> [1] "0.93" "0.94" "0.95" "0.96" "0.97"

# Both of these functions can offset the
# starting point...
seq_endpoint(from = 14.2, to = 15, offset_from = 4)
#> [1] "14.6" "14.7" "14.8" "14.9" "15.0"
seq_distance(from = 14.2, offset_from = 4)
#>  [1] "14.6" "14.7" "14.8" "14.9" "15.0" "15.1" "15.2" "15.3" "15.4" "15.5"

# ...but only `seq_endpoint()` can offset the
# endpoint, because of its `to` argument:
seq_endpoint(from = 9.5, to = 10, offset_to = 2)
#> [1] "9.5"  "9.6"  "9.7"  "9.8"  "9.9"  "10.0" "10.1" "10.2"

# In return, `seq_distance()` can reverse its direction:
seq_distance(from = 20.03, dir = -1)
#>  [1] "20.03" "20.02" "20.01" "20.00" "19.99" "19.98" "19.97" "19.96" "19.95"
#> [10] "19.94"

# Both functions have a `_df` variant that returns
# a data frame. Arguments are the same but with a
# dot, and further columns can be added as in
# `tibble::tibble()`:
seq_endpoint_df(.from = 4.7, .to = 5, n = 20)
#> # A tibble: 4 × 2
#>   x         n
#>   <chr> <dbl>
#> 1 4.7      20
#> 2 4.8      20
#> 3 4.9      20
#> 4 5.0      20
seq_distance_df(.from = 0.43, .length_out = 5, sd = 0.08)
#> # A tibble: 5 × 2
#>   x        sd
#>   <chr> <dbl>
#> 1 0.43   0.08
#> 2 0.44   0.08
#> 3 0.45   0.08
#> 4 0.46   0.08
#> 5 0.47   0.08
```
