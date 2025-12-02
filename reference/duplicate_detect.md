# Detect duplicate values

**\[superseded\]**

`duplicate_detect()` is superseded because it's less informative than
[`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
and
[`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md).
Use these functions instead.

For every value in a vector or data frame, `duplicate_detect()` tests
whether there is at least one identical value. Test results are
presented next to every value.

This function is a blunt tool designed for initial data checking. Don't
put too much weight on its results.

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results.

## Usage

``` r
duplicate_detect(x, ignore = NULL, colname_end = "dup")
```

## Arguments

- x:

  Vector or data frame.

- ignore:

  Optionally, a vector of values that should not be checked. In the test
  result columns, they will be marked `NA`.

- colname_end:

  String. Name ending of the logical test result columns. Default is
  `"dup"`.

## Value

A tibble (data frame). It has all the columns from `x`, and to each of
these columns' right, the corresponding test result column.

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
following `duplicate_detect()`. It returns a tibble with these columns —

- `term`: The original data frame's variables.

- `dup_count`: Number of "duplicated" values of that `term` variable:
  those which have at least one duplicate anywhere in the data frame.

- `total`: Number of all non-`NA` values of that `term` variable.

- `dup_rate`: Rate of "duplicated" values of that `term` variable.

The final row, `.total`, summarizes across all other rows: It adds up
the `dup_count` and `total_count` columns, and calculates the mean of
the `dup_rate` column.

## See also

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  to count instances of a value instead of just stating whether it is
  duplicated.

- [`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
  for a frequency table.

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  to check each combination of columns for duplicates.

- `janitor::get_dupes()` to search for duplicate rows.

## Examples

``` r
# Find duplicate values in a data frame...
duplicate_detect(x = pigs4)
#> # A tibble: 5 × 6
#>   snout snout_dup tail  tail_dup wings wings_dup
#>   <chr> <lgl>     <chr> <lgl>    <chr> <lgl>    
#> 1 4.73  FALSE     6.88  FALSE    6.09  FALSE    
#> 2 8.13  TRUE      7.33  FALSE    8.27  FALSE    
#> 3 4.22  TRUE      5.17  TRUE     4.40  FALSE    
#> 4 4.22  TRUE      7.57  FALSE    5.92  FALSE    
#> 5 5.17  TRUE      8.13  TRUE     5.17  TRUE     

# ...or in a single vector:
duplicate_detect(x = pigs4$snout)
#> # A tibble: 5 × 2
#>   value value_dup
#>   <chr> <lgl>    
#> 1 4.73  FALSE    
#> 2 8.13  FALSE    
#> 3 4.22  TRUE     
#> 4 4.22  TRUE     
#> 5 5.17  FALSE    

# Summary statistics with `audit()`:
pigs4 %>%
  duplicate_detect() %>%
  audit()
#> # A tibble: 4 × 4
#>   term   dup_count total_count dup_rate
#>   <chr>      <int>       <int>    <dbl>
#> 1 snout          4           5    0.8  
#> 2 tail           2           5    0.4  
#> 3 wings          1           5    0.2  
#> 4 .total         7          15    0.467

# Any values can be ignored:
pigs4 %>%
  duplicate_detect(ignore = c(8.131, 7.574))
#> # A tibble: 5 × 6
#>   snout snout_dup tail  tail_dup wings wings_dup
#>   <chr> <lgl>     <chr> <lgl>    <chr> <lgl>    
#> 1 4.73  FALSE     6.88  FALSE    6.09  FALSE    
#> 2 8.13  TRUE      7.33  FALSE    8.27  FALSE    
#> 3 4.22  TRUE      5.17  TRUE     4.40  FALSE    
#> 4 4.22  TRUE      7.57  FALSE    5.92  FALSE    
#> 5 5.17  TRUE      8.13  TRUE     5.17  TRUE     
```
