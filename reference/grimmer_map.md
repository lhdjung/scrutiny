# GRIMMER-test many cases at once

Call `grimmer_map()` to GRIMMER-test any number of combinations of mean,
standard deviation, sample size, and number of items. Mapping function
for GRIMMER testing.

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results. Visualize results using
[`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md),
as with GRIM results.

## Usage

``` r
grimmer_map(
  data,
  items = 1,
  merge_items = TRUE,
  x = NULL,
  sd = NULL,
  n = NULL,
  show_reason = TRUE,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE,
  tolerance = .Machine$double.eps^0.5
)
```

## Arguments

- data:

  Data frame with columns `x`, `sd`, `n`, and optionally `items` (see
  documentation for
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md)). Any
  other columns in `data` will be returned alongside GRIMMER test
  results.

- items:

  Integer. If there is no `items` column in `data`, this specifies the
  number of items composing the `x` and `sd` values. Default is `1`, the
  most common case.

- merge_items:

  Logical. If `TRUE` (the default), there will be no `items` column in
  the output. Instead, values from an `items` column or argument will be
  multiplied with values in the `n` column. This is only for
  presentation and does not affect test results.

- x, sd, n:

  Optionally, specify these arguments as column names in `data`.

- show_reason:

  Logical (length 1). Should there be a `reason` column that shows the
  reasons for inconsistencies and `"Passed all"` for consistent values?
  Default is `FALSE`. See below for reference.

- rounding, threshold, symmetric, tolerance:

  Further parameters of GRIMMER testing; see documentation for
  [`grimmer()`](https://lhdjung.github.io/scrutiny/reference/grimmer.md).

## Value

A tibble with these columns –

- `x`, `sd`, `n`: the inputs.

- `consistency`: GRIMMER consistency of `x`, `n`, and `items`.

- `reason`: If consistent, `"Passed all"`. If inconsistent, it says
  which test was failed (see below).

- `<extra>`: any columns from `data` other than `x`, `n`, and `items`.

The `reason` columns refers to GRIM and the three GRIMMER tests (Allard
2018). Briefly, these are:

1.  The reconstructed sum of squared observations must be a whole
    number.

2.  The reconstructed SD must match the reported one.

3.  The parity of the reconstructed sum of squared observations must
    match the parity of the reconstructed sum of integers of which the
    reported means are fractions; i.e., either both are even or both are
    odd.

The tibble has the `scr_grimmer_map` class, which is recognized by the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic. It also has the `scr_grim_map` class, so it can be visualized
by
[`grim_plot()`](https://lhdjung.github.io/scrutiny/reference/grim_plot.md).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md), so
you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `grimmer_map()` to get a summary of `grimmer_map()`'s results.
It is a tibble with a single row and these columns –

1.  `incons_cases`: number of GRIMMER-inconsistent value sets.

2.  `all_cases`: total number of value sets.

3.  `incons_rate`: proportion of GRIMMER-inconsistent value sets.

4.  `fail_grim`: number of value sets that fail the GRIM test.

5.  `fail_test1`: number of value sets that fail the first GRIMMER test
    (see below).

6.  `fail_test2`: number of value sets that fail the second GRIMMER
    test.

7.  `fail_test3`: number of value sets that fail the third GRIMMER test.

The `reason` columns refers to the three GRIMMER tests (see Allard
2018). These are:

1.  The reconstructed sum of squared observations must be a whole
    number.

2.  The reconstructed SD must match the reported one.

3.  The parity of the reconstructed sum of squared observations must
    match the parity of the reconstructed sum of integers of which the
    reported means are fractions; i.e., either both are even or both are
    odd.

## References

Allard, A. (2018). Analytic-GRIMMER: a new way of testing the
possibility of standard deviations.
https://aurelienallard.netlify.app/post/anaytic-grimmer-possibility-standard-deviations/

Anaya, J. (2016). The GRIMMER test: A method for testing the validity of
reported measures of variability. *PeerJ Preprints.*
https://peerj.com/preprints/2400v1/

## Examples

``` r
# Use `grimmer_map()` on data like these:
pigs5
#> # A tibble: 12 × 3
#>    x     sd        n
#>    <chr> <chr> <dbl>
#>  1 7.22  5.30     38
#>  2 4.74  6.55     31
#>  3 5.23  2.55     35
#>  4 2.57  2.57     30
#>  5 6.77  2.18     33
#>  6 2.68  2.59     34
#>  7 7.01  6.68     35
#>  8 7.38  3.65     32
#>  9 3.14  5.32     33
#> 10 6.89  4.18     37
#> 11 5.00  2.18     31
#> 12 0.24  6.43     34

# The `consistency` column shows whether
# the values to its left are GRIMMER-consistent.
# If they aren't, the `reason` column says why:
pigs5 %>%
  grimmer_map()
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> # A tibble: 12 × 5
#>    x     sd        n consistency reason                       
#>    <chr> <chr> <dbl> <lgl>       <chr>                        
#>  1 7.22  5.30     38 FALSE       GRIM inconsistent            
#>  2 4.74  6.55     31 TRUE        Passed all                   
#>  3 5.23  2.55     35 FALSE       GRIMMER inconsistent (test 3)
#>  4 2.57  2.57     30 TRUE        Passed all                   
#>  5 6.77  2.18     33 FALSE       GRIM inconsistent            
#>  6 2.68  2.59     34 TRUE        Passed all                   
#>  7 7.01  6.68     35 FALSE       GRIM inconsistent            
#>  8 7.38  3.65     32 TRUE        Passed all                   
#>  9 3.14  5.32     33 FALSE       GRIM inconsistent            
#> 10 6.89  4.18     37 TRUE        Passed all                   
#> 11 5.00  2.18     31 TRUE        Passed all                   
#> 12 0.24  6.43     34 TRUE        Passed all                   

# Get summaries with `audit()`:
pigs5 %>%
  grimmer_map() %>%
  audit()
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> Warning: False-positive GRIMMER results possible.
#> ! I became aware of a bug in the `grimmer*()` functions.
#> ✖ GRIMMER's test 3 can flag consistent values as inconsistent.
#> ✖ For now, please use `show_reason = TRUE` and interpret results of test 3 with
#>   care. (The first two tests and GRIM are not affected.)
#> ℹ The next version of scrutiny will provide a fix. Many apologies.
#> ℹ For more information, see <https://github.com/lhdjung/scrutiny/issues/80>
#> # A tibble: 1 × 7
#>   incons_cases all_cases incons_rate fail_grim fail_test1 fail_test2 fail_test3
#>          <int>     <int>       <dbl>     <int>      <int>      <int>      <int>
#> 1            5        12       0.417         4          0          0          1
```
