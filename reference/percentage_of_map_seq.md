# Test percentages against their counts with values around the reported ones

**\[experimental\]**

`percentage_of_map_seq()` varies the reported counts, percentages, and
totals a few steps up and down and applies
[`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
to each resulting value set. This shows whether a small error in one of
them could explain an inconsistency.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
percentage_of_map_seq(
  data,
  digits_percentage,
  absolute = NULL,
  percentage = NULL,
  n = NULL,
  ...,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  var = c("absolute", "percentage", "n"),
  dispersion = 1:5,
  out_min = "auto",
  out_max = "auto",
  include_reported = FALSE,
  include_consistent = FALSE
)
```

## Arguments

- data:

  A data frame that
  [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
  could take.

- digits_percentage:

  Integer. The number of decimal places in `percentage`, including
  trailing zeros. There is no default, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for the whole
  column or one number per row of `data`. If left out, it is taken from
  a `digits_percentage` column of `data`. It also sets the step size for
  dispersing `percentage`.

- absolute, percentage, n:

  Optionally, the names of the columns in `data` that contain these
  values, if they differ from the defaults.

- ...:

  These dots must be empty. Every argument after them must be named in
  full.

- rounding, threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

- var:

  String. The variables to disperse. Default is
  `c("absolute", "percentage", "n")`.

- dispersion:

  Numeric. Steps up and down from the reported values, in units of the
  last decimal place. Default is `1:5`.

- out_min, out_max:

  Limits on the dispersed values. Both default to `"auto"`, which keeps
  `absolute` and `percentage` at 0 or above and `n` at 1 or above. A
  number applies to every dispersed variable, and `NULL` removes the
  limit.

- include_reported:

  Logical. Should the reported values themselves be part of the
  sequences? Default is `FALSE`.

- include_consistent:

  Logical. Should consistent value sets be dispersed as well, not just
  inconsistent ones? Default is `FALSE`.

## Value

A tibble with the columns of
[`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
output plus `diff_var`, `case`, and `var`, as in
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).
As there, the other columns of `data` are dropped.

## Examples

``` r
df <- tibble::tibble(
  absolute = c(40, 37, 25),
  percentage = c(33, 31, 20),
  n = 120
)

df |>
  percentage_of_map_seq(digits_percentage = 0) |>
  audit()
#> # A tibble: 1 × 17
#>   absolute percentage     n consistency hits_total hits_absolute hits_percentage
#>      <int>      <dbl> <int> <lgl>            <int>         <int>           <int>
#> 1       25         20   120 FALSE                6             1               1
#> # ℹ 10 more variables: hits_n <int>, diff_absolute <int>,
#> #   diff_absolute_up <int>, diff_absolute_down <int>, diff_percentage <int>,
#> #   diff_percentage_up <int>, diff_percentage_down <int>, diff_n <int>,
#> #   diff_n_up <int>, diff_n_down <int>
```
