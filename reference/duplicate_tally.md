# Count duplicates at each observation

For every value in a vector or data frame, `duplicate_tally()` counts
how often it appears in total. Tallies are presented next to each value.

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results.

## Usage

``` r
duplicate_tally(x, ignore = NULL, colname_end = "n")
```

## Arguments

- x:

  Vector or data frame.

- ignore:

  Optionally, a vector of values that should not be checked. In the test
  result columns, they will be marked `NA`.

- colname_end:

  String. Name ending of the logical test result columns. Default is
  `"n"`.

## Value

A tibble (data frame). It has all the columns from `x`, and to each of
these columns' right, the corresponding tally column.

The tibble has the `scr_dup_detect` class, which is recognized by the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic.

## Details

This function is not very informative with many input values that only
have a few characters each. Many of them may have duplicates just by
chance. For example, in R's built-in `iris` data set, 99% of values have
duplicates.

In general, the fewer values and the more characters per value, the more
significant the results.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic, so you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `duplicate_tally()`. It returns a tibble with summary
statistics.

## See also

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  for a frequency table.

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  to check each combination of columns for duplicates.

- `janitor::get_dupes()` to search for duplicate rows.

## Examples

``` r
# Tally duplicate values in a data frame...
duplicate_tally(x = pigs4)
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13        2 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.40        1
#> 4 4.22        2 7.57       1 5.92        1
#> 5 5.17        3 8.13       2 5.17        3

# ...or in a single vector:
duplicate_tally(x = pigs4$snout)
#> # A tibble: 5 × 2
#>   value value_n
#>   <chr>   <int>
#> 1 4.73        1
#> 2 8.13        1
#> 3 4.22        2
#> 4 4.22        2
#> 5 5.17        1

# Summary statistics with `audit()`:
pigs4 %>%
  duplicate_tally() %>%
  audit()
#> # A tibble: 4 × 8
#>   term     mean    sd median   min   max na_count na_rate
#>   <chr>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 snout_n  2    0.707      2     1     3        0       0
#> 2 tail_n   1.6  0.894      1     1     3        0       0
#> 3 wings_n  1.4  0.894      1     1     3        0       0
#> 4 .total   1.67 0.816      1     1     3        0       0

# Any values can be ignored:
pigs4 %>%
  duplicate_tally(ignore = c(8.131, 7.574))
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13        2 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.40        1
#> 4 4.22        2 7.57       1 5.92        1
#> 5 5.17        3 8.13       2 5.17        3
```
