# Count duplicates at each observation

For every value in a vector or data frame, `duplicate_tally()` counts
how often it occurs in the whole input, and reports the count next to
the value.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
duplicate_tally(x, ignore = NULL, colname_end = "n")
```

## Arguments

- x:

  Vector, matrix, or data frame. Blank strings, `"NA"`, and `"NaN"`
  count as missing.

- ignore:

  Optionally, a vector of values not to check. Their tally is `NA`, as
  is that of missing values.

- colname_end:

  String. Suffix of the tally columns. Default is `"n"`.

## Value

A tibble with the columns of `x` (a `value` column if `x` is a vector),
each followed by its tally column. It has the class
`scrutiny_dup_tally`, which is recognized by
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md).

## Details

Values with few characters are often duplicated by chance: in `iris`,
99% of values have a duplicate. Duplicates are more meaningful when
there are fewer values and they have more characters.

Values are compared and returned as strings, so `1` and `"1"` are
duplicates, but `2` and `"2.0"` are not: `2.0` becomes `"2"`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`duplicate_tally()` output returns summary statistics for each tally
column, with a `.total` row across all of them. It finds these columns
by the names that `duplicate_tally()` gave them, so a renamed one is
left out.

## See also

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  for a frequency table.

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  for duplicates between pairs of columns.

- `janitor::get_dupes()` for duplicate rows.

## Examples

``` r
duplicate_tally(x = pigs4)
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13        2 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.4         1
#> 4 4.22        2 7.57       1 5.92        1
#> 5 5.17        3 8.13       2 5.17        3

duplicate_tally(x = pigs4$snout)
#> # A tibble: 5 × 2
#>   value value_n
#>   <chr>   <int>
#> 1 4.73        1
#> 2 8.13        1
#> 3 4.22        2
#> 4 4.22        2
#> 5 5.17        1

pigs4 |>
  duplicate_tally() |>
  audit()
#> # A tibble: 4 × 8
#>   term     mean    sd median   min   max na_count na_rate
#>   <chr>   <dbl> <dbl>  <int> <int> <int>    <int>   <dbl>
#> 1 snout_n  2    0.707      2     1     3        0       0
#> 2 tail_n   1.6  0.894      1     1     3        0       0
#> 3 wings_n  1.4  0.894      1     1     3        0       0
#> 4 .total   1.67 0.816      1     1     3        0       0

pigs4 |>
  duplicate_tally(ignore = c(8.13, 7.57))
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13       NA 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.4         1
#> 4 4.22        2 7.57      NA 5.92        1
#> 5 5.17        3 8.13      NA 5.17        3
```
