# Use DEBIT with hypothetical group sizes

`debit_map_total_n()` extends DEBIT to cases where only group means and
standard deviations (SDs) were reported, not group sizes.

The function is analogous to
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
and
[`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md),
relying on the same infrastructure.

## Usage

``` r
debit_map_total_n(
  data,
  x1 = NULL,
  x2 = NULL,
  sd1 = NULL,
  sd2 = NULL,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL,
  constant_index = NULL,
  ...
)
```

## Arguments

- data:

  Data frame with string columns `x1`, `x2`, `sd1`, and `sd2`, as well
  as numeric column `n`. The first two are reported group means. `sd1`
  and `sd2` are reported group SDs. `n` is the reported total sample
  size. It is not very important whether a value is in `x1` or in `x2`
  because, after the first round of tests, the function switches roles
  between `x1` and `x2`, and reports the outcomes both ways. The same
  applies to `sd1` and `sd2`. However, do make sure the `x*` and `sd*`
  values are paired accurately, as reported.

- x1, x2, sd1, sd2:

  Optionally, specify these arguments as column names in `data`.

- dispersion:

  Numeric. Steps up and down from half the `n` values. Default is `0:5`,
  i.e., half `n` itself followed by five steps up and down.

- n_min:

  Numeric. Minimal group size. Default is 1.

- n_max:

  Numeric. Maximal group size. Default is `NULL`, i.e., no maximum.

- constant:

  Optionally, add a length-2 vector or a list of length-2 vectors (such
  as a data frame with exactly two rows) to accompany the pairs of
  dispersed values. Default is `NULL`, i.e., no constant values.

- constant_index:

  Integer (length 1). Index of `constant` or the first `constant` column
  in the output tibble. If `NULL` (the default), `constant` will go to
  the right of `n_change`.

- ...:

  Arguments passed down to
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).

## Value

A tibble with these columns:

- `x` and `sd`, the group-wise reported input statistics, are repeated
  in row pairs.

- `n` is dispersed from half the input `n`, with `n_change` tracking the
  differences.

- `both_consistent` flags scenarios where both reported `x` and `sd`
  values are consistent with the hypothetical `n` values.

- `case` corresponds to the row numbers of the input data frame.

- `dir` is `"forth"` in the first half of rows and `"back"` in the
  second half. `"forth"` means that `x2` and `sd2` from the input are
  paired with the larger dispersed `n`, whereas `"back"` means that `x1`
  and `sd1` are paired with the larger dispersed `n`.

- Other columns from
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
  are preserved.

## Summaries with [`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)

You can call
[`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
following up on `debit_map_total_n()` to get a tibble with summary
statistics. It will have these columns:

- `x1`, `x2`, `sd1`, `sd2`, and `n` are the original inputs.

- `hits_total` is the number of scenarios in which all of `x1`, `x2`,
  `sd1`, and `sd2` are DEBIT-consistent. It is the sum of `hits_forth`
  and `hits_back` below.

- `hits_forth` is the number of both-consistent cases that result from
  pairing `x2` and `sd2` with the larger dispersed `n` value.

- `hits_back` is the same, except `x1` and `sd1` are paired with the
  larger dispersed `n` value.

- `scenarios_total` is the total number of test scenarios, whether or
  not both `x1` and `sd1` as well as `x2` and `sd2` are
  DEBIT-consistent.

- `hit_rate` is the ratio of `hits_total` to `scenarios_total`.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following
[`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md)
to summarize results even further.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

Heathers, J. A. J., & Brown, N. J. L. (2019). DEBIT: A Simple
Consistency Test For Binary Data. https://osf.io/5vb3u/.

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
which created the present function using
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md).

## Examples

``` r
# Run `debit_map_total_n()` on data like these:
df <- tibble::tribble(
  ~x1,  ~x2,  ~sd1,  ~sd2,  ~n,
  "0.30", "0.28", "0.17", "0.10", 70,
  "0.41", "0.39", "0.09", "0.15", 65
)
df
#> # A tibble: 2 × 5
#>   x1    x2    sd1   sd2       n
#>   <chr> <chr> <chr> <chr> <dbl>
#> 1 0.30  0.28  0.17  0.10     70
#> 2 0.41  0.39  0.09  0.15     65

debit_map_total_n(df)
#> # A tibble: 48 × 15
#>    x     sd        n n_change consistency both_consistent rounding   sd_lower
#>    <chr> <chr> <int>    <int> <lgl>       <lgl>           <chr>         <dbl>
#>  1 0.30  0.17     35        0 FALSE       FALSE           up_or_down    0.165
#>  2 0.28  0.10     35        0 FALSE       FALSE           up_or_down    0.095
#>  3 0.30  0.17     34       -1 FALSE       FALSE           up_or_down    0.165
#>  4 0.28  0.10     36        1 FALSE       FALSE           up_or_down    0.095
#>  5 0.30  0.17     33       -2 FALSE       FALSE           up_or_down    0.165
#>  6 0.28  0.10     37        2 FALSE       FALSE           up_or_down    0.095
#>  7 0.30  0.17     32       -3 FALSE       FALSE           up_or_down    0.165
#>  8 0.28  0.10     38        3 FALSE       FALSE           up_or_down    0.095
#>  9 0.30  0.17     31       -4 FALSE       FALSE           up_or_down    0.165
#> 10 0.28  0.10     39        4 FALSE       FALSE           up_or_down    0.095
#> # ℹ 38 more rows
#> # ℹ 7 more variables: sd_incl_lower <lgl>, sd_upper <dbl>, sd_incl_upper <lgl>,
#> #   x_lower <dbl>, x_upper <dbl>, case <int>, dir <fct>
```
