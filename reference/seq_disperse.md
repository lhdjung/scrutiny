# Sequence generation with dispersion at decimal level

`seq_disperse()` creates a sequence around a given number. It goes a
specified number of steps up and down from it. Step size depends on the
number's decimal places. For example, `7.93` will be surrounded by
values like `7.91`, `7.92`, and `7.94`, `7.95`, etc.

`seq_disperse_df()` is a variant that creates a data frame. Further
columns can be added as in
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
Regular arguments are the same as in `seq_disperse()`, but with a dot
before each.

## Usage

``` r
seq_disperse(
  from,
  by = NULL,
  dispersion = 1:5,
  offset_from = 0L,
  out_min = "auto",
  out_max = NULL,
  string_output = TRUE,
  include_reported = TRUE,
  track_diff_var = FALSE,
  track_var_change = deprecated()
)

seq_disperse_df(
  .from,
  .by = NULL,
  ...,
  .dispersion = 1:5,
  .offset_from = 0L,
  .out_min = "auto",
  .out_max = NULL,
  .string_output = TRUE,
  .include_reported = TRUE,
  .track_diff_var = FALSE,
  .track_var_change = FALSE
)
```

## Arguments

- from, .from:

  Numeric (or string coercible to numeric). Starting point of the
  sequence.

- by, .by:

  Numeric. Step size of the sequence. If not set, inferred
  automatically. Default is `NULL`.

- dispersion, .dispersion:

  Numeric. Vector that determines the steps up and down, starting at
  `from` (or `.from`, respectively) and proceeding on the level of its
  last decimal place. Default is `1:5`, i.e., five steps up and down.

- offset_from, .offset_from:

  Integer. If set to a non-zero number, the starting point will be
  offset by that many units on the level of the last decimal digit.
  Default is `0`.

- out_min, .out_min, out_max, .out_max:

  If specified, output will be restricted so that it's not below
  `out_min` or above `out_max`. Defaults are `"auto"` for `out_min`,
  i.e., a minimum of one decimal unit above zero; and `NULL` for
  `out_max`, i.e., no maximum.

- string_output, .string_output:

  Logical or string. If `TRUE` (the default), the output is a string
  vector. Decimal places are then padded with zeros to match `from`'s
  number of decimal places. `"auto"` works like `TRUE` if and only if
  `from` (`.from`) is a string.

- include_reported, .include_reported:

  Logical. Should `from` (`.from`) itself be part of the sequence built
  around it? Default is `TRUE` for the sake of continuity, but this can
  be misleading if the focus is on the dispersed values, as opposed to
  the input.

- track_diff_var, .track_diff_var:

  Logical. In `seq_disperse()`, ignore this argument. In
  `seq_disperse_df()`, default is `TRUE`, which creates the `"diff_var"`
  output column.

- track_var_change, .track_var_change:

  **\[deprecated\]** Renamed to `track_diff_var` / `.track_diff_var`.

- ...:

  Further columns, added as in
  [`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html).
  Only in `seq_disperse_df()`.

## Value

- `seq_disperse()` returns a string vector by default
  (`string_output = TRUE`) and a numeric vector otherwise.

- `seq_disperse_df()` returns a tibble (data frame). The sequence is
  stored in the `x` column. `x` is string by default
  (`.string_output = TRUE`), numeric otherwise. Other columns might have
  been added via the dots (`...`).

## Details

Unlike
[`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
and friends, the present functions don't necessarily return continuous
or even regular sequences. The greater flexibility is due to the
`dispersion` (`.dispersion`) argument, which takes any numeric vector.
By default, however, the output sequence is regular and continuous.

Underlying this difference is the fact that `seq_disperse()` and
`seq_disperse_df()` do not wrap around
[`base::seq()`](https://rdrr.io/r/base/seq.html), although they are
otherwise similar to
[`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
and friends.

## See also

Conceptually, `seq_disperse()` is a blend of two function families:
those around
[`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
and those around
[`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md).
The present functions were originally conceived for `seq_disperse_df()`
to be a helper within the
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
implementation.

## Examples

``` r
# Basic usage:
seq_disperse(from = 4.02)
#>  [1] "3.97" "3.98" "3.99" "4.00" "4.01" "4.02" "4.03" "4.04" "4.05" "4.06"
#> [11] "4.07"

# If trailing zeros don't matter,
# the output can be numeric:
seq_disperse(from = 4.02, string_output = FALSE)
#>  [1] 3.97 3.98 3.99 4.00 4.01 4.02 4.03 4.04 4.05 4.06 4.07

# Control steps up and down with
# `dispersion` (default is `1:5`):
seq_disperse(from = 4.02, dispersion = 1:10)
#>  [1] "3.92" "3.93" "3.94" "3.95" "3.96" "3.97" "3.98" "3.99" "4.00" "4.01"
#> [11] "4.02" "4.03" "4.04" "4.05" "4.06" "4.07" "4.08" "4.09" "4.10" "4.11"
#> [21] "4.12"

# Sequences might be discontinuous...
disp1 <- seq(from = 2, to = 10, by = 2)
seq_disperse(from = 4.02, dispersion = disp1)
#>  [1] "3.92" "3.94" "3.96" "3.98" "4.00" "4.02" "4.04" "4.06" "4.08" "4.10"
#> [11] "4.12"

# ...or even irregular:
disp2 <- c(2, 3, 7)
seq_disperse(from = 4.02, dispersion = disp2)
#> [1] "3.95" "3.99" "4.00" "4.02" "4.04" "4.05" "4.09"

# The data fame variant supports further
# columns added as in `tibble::tibble()`:
seq_disperse_df(.from = 4.02, n = 45)
#> # A tibble: 11 × 2
#>    x         n
#>    <chr> <dbl>
#>  1 3.97     45
#>  2 3.98     45
#>  3 3.99     45
#>  4 4.00     45
#>  5 4.01     45
#>  6 4.02     45
#>  7 4.03     45
#>  8 4.04     45
#>  9 4.05     45
#> 10 4.06     45
#> 11 4.07     45
```
