# GRIM-testing with dispersed inputs

`grim_map_seq()` performs GRIM-testing with values surrounding the input
values. This provides an easy and powerful way to assess whether small
errors in computing or reporting may be responsible for GRIM
inconsistencies in published statistics.

Call
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
on the results for summary statistics.

## Usage

``` r
grim_map_seq(
  data,
  x = NULL,
  n = NULL,
  var = Inf,
  dispersion = 1:5,
  out_min = "auto",
  out_max = NULL,
  include_reported = FALSE,
  include_consistent = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame that
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  could take.

- x, n:

  Optionally, specify these arguments as column names in `data`.

- var:

  String. Names of the columns that will be dispersed. Default is
  `c("x", "n")`.

- dispersion:

  Numeric. Sequence with steps up and down from the `var` inputs. It
  will be adjusted to these values' decimal levels. For example, with a
  reported `8.34`, the step size is `0.01`. Default is `1:5`, for five
  steps up and down.

- out_min, out_max:

  If specified, output will be restricted so that it's not below
  `out_min` or above `out_max`. Defaults are `"auto"` for `out_min`,
  i.e., a minimum of one decimal unit above zero; and `NULL` for
  `out_max`, i.e., no maximum.

- include_reported:

  Logical. Should the reported values themselves be included in the
  sequences originating from them? Default is `FALSE` because this might
  be redundant and bias the results.

- include_consistent:

  Logical. Should the function also process consistent cases (from among
  those reported), not just inconsistent ones? Default is `FALSE`
  because the focus should be on clarifying inconsistencies.

- ...:

  Arguments passed down to
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

## Value

A tibble (data frame) with detailed test results.

## Summaries with [`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

You can call
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
following `grim_map_seq()`. It will return a data frame with these
columns:

- `x` and `n` are the original inputs, tested for `consistency` here.

- `hits_total` is the total number of GRIM-consistent value sets found
  within the specified `dispersion` range.

- `hits_x` is the number of GRIM-consistent value sets found by varying
  `x`.

- Accordingly with `n` and `hits_n`.

- (Note that any consistent reported cases will be counted by the
  `hits_*` columns if both `include_reported` and `include_consistent`
  are set to `TRUE`.)

- `diff_x` reports the absolute difference between `x` and the next
  consistent dispersed value (in dispersion steps, not the actual
  numeric difference). `diff_x_up` and `diff_x_down` report the
  difference to the next higher or lower consistent value, respectively.

- `diff_n`, `diff_n_up`, and `diff_n_down` do the same for `n`.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
to summarize results even further. It's mostly self-explaining, but
`na_count` and `na_rate` are the number and rate of times that a
difference could not be computed because of a lack of corresponding hits
within the `dispersion` range.

## Examples

``` r
# `grim_map_seq()` can take any input
# that `grim_map()` can take:
pigs1
#> # A tibble: 12 × 2
#>    x         n
#>    <chr> <dbl>
#>  1 7.22     32
#>  2 4.74     25
#>  3 5.23     29
#>  4 2.57     24
#>  5 6.77     27
#>  6 2.68     28
#>  7 7.01     29
#>  8 7.38     26
#>  9 3.14     27
#> 10 6.89     31
#> 11 5.00     25
#> 12 0.24     28

# All the results:
out <- grim_map_seq(pigs1, include_consistent = TRUE)
out
#> # A tibble: 240 × 7
#>    x         n consistency probability diff_var  case var  
#>    <chr> <int> <lgl>             <dbl>    <int> <int> <chr>
#>  1 7.17     32 FALSE              0.68       -5     1 x    
#>  2 7.18     32 FALSE              0.68       -4     1 x    
#>  3 7.19     32 TRUE               0.68       -3     1 x    
#>  4 7.20     32 FALSE              0.68       -2     1 x    
#>  5 7.21     32 FALSE              0.68       -1     1 x    
#>  6 7.23     32 FALSE              0.68        1     1 x    
#>  7 7.24     32 FALSE              0.68        2     1 x    
#>  8 7.25     32 TRUE               0.68        3     1 x    
#>  9 7.26     32 FALSE              0.68        4     1 x    
#> 10 7.27     32 FALSE              0.68        5     1 x    
#> # ℹ 230 more rows

# Case-wise summaries with `audit_seq()`
# can be more important than the raw results:
out %>%
  audit_seq()
#> # A tibble: 12 × 12
#>    x         n consistency hits_total hits_x hits_n diff_x diff_x_up diff_x_down
#>    <chr> <int> <lgl>            <int>  <int>  <int>  <int>     <int>       <int>
#>  1 7.22     32 TRUE                 5      2      3      3         3          -3
#>  2 4.74     25 FALSE                4      2      2      2         2          -2
#>  3 5.23     29 FALSE                6      3      3      1         1          -2
#>  4 2.57     24 FALSE                6      3      3      1         1          -3
#>  5 6.77     27 FALSE                7      3      4      1         1          -3
#>  6 2.68     28 TRUE                 4      2      2      3         3          -4
#>  7 7.01     29 FALSE                3      3      0      1         2          -1
#>  8 7.38     26 TRUE                 5      2      3      3         4          -3
#>  9 3.14     27 FALSE                6      3      3      1         1          -3
#> 10 6.89     31 FALSE                8      4      4      1         1          -2
#> 11 5.00     25 TRUE                12      2     10      4         4          -4
#> 12 0.24     28 FALSE                6      3      3      1         1          -3
#> # ℹ 3 more variables: diff_n <int>, diff_n_up <int>, diff_n_down <int>
```
