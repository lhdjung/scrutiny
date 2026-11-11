# Duplication analysis

Repeated values in a dataset can be a sign of copied or fabricated data.
scrutiny has three functions for counting duplicates, each with an
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
method for summaries. The examples use the `pigs4` dataset:

``` r

pigs4
#> # A tibble: 5 × 3
#>   snout  tail wings
#>   <dbl> <dbl> <dbl>
#> 1  4.73  6.88  6.09
#> 2  8.13  7.33  8.27
#> 3  4.22  5.17  4.4 
#> 4  4.22  7.57  5.92
#> 5  5.17  8.13  5.17
```

## Frequency tabulation with `duplicate_count()`

A good first step is a frequency table:

``` r

pigs4 |>
  duplicate_count()
#> # A tibble: 11 × 4
#>    value frequency locations          locations_n
#>    <chr>     <int> <chr>                    <int>
#>  1 5.17          3 snout, tail, wings           3
#>  2 4.22          2 snout                        1
#>  3 8.13          2 snout, tail                  2
#>  4 4.73          1 snout                        1
#>  5 6.88          1 tail                         1
#>  6 7.33          1 tail                         1
#>  7 7.57          1 tail                         1
#>  8 4.4           1 wings                        1
#>  9 5.92          1 wings                        1
#> 10 6.09          1 wings                        1
#> 11 8.27          1 wings                        1
```

Each distinct `value` is one row, ordered by its `frequency`.
`locations` names the columns in which the value appears, and
`locations_n` counts them. For example, `5.17` appears 3 times, once
each in `snout`, `tail`, and `wings`, so `locations_n` is `3`. `4.22`
appears twice, but both times in `snout`, so `locations_n` is `1`.

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes `frequency` and `locations_n`:

``` r

pigs4 |>
  duplicate_count() |>
  audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <int> <int> <int>    <int>   <dbl>
#> 1 frequency    1.36 0.674      1     1     3        0       0
#> 2 locations_n  1.27 0.647      1     1     3        0       0
```

## Counting by column pair with `duplicate_count_colpair()`

A sequence of values may be repeated across columns.
[`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
compares every pair of columns:

``` r

pigs4 |>
  duplicate_count_colpair()
#> # A tibble: 3 × 7
#>   x     y     count total_x total_y rate_x rate_y
#>   <chr> <chr> <int>   <int>   <int>  <dbl>  <dbl>
#> 1 snout tail      2       5       5    0.4    0.4
#> 2 snout wings     1       5       5    0.2    0.2
#> 3 tail  wings     1       5       5    0.2    0.2
```

`count` is the number of values of `x` that also appear in `y`.
`total_x` and `total_y` are the numbers of non-missing values in each
column, `rate_x` is the proportion of `x` values that also appear in
`y`, and `rate_y` the proportion of `y` values that also appear in `x`.
Without missing values, `total_x` equals `total_y`, and `rate_x` equals
`rate_y` unless a value repeats within a column.

Here, `snout` and `tail` overlap the most: 2 of their 5 values are the
same, a rate of 0.4. Summarize with
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md):

``` r

pigs4 |>
  duplicate_count_colpair() |>
  audit()
#> # A tibble: 5 × 8
#>   term     mean    sd median   min   max na_count na_rate
#>   <chr>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <int>   <dbl>
#> 1 count   1.33  0.577    1     1     2          0       0
#> 2 total_x 5     0        5     5     5          0       0
#> 3 total_y 5     0        5     5     5          0       0
#> 4 rate_x  0.267 0.115    0.2   0.2   0.4        0       0
#> 5 rate_y  0.267 0.115    0.2   0.2   0.4        0       0
```

## Counting by observation with `duplicate_tally()`

[`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
keeps the shape of the data. Next to each column, it adds one ending in
`_n` that counts how often each value appears in the data frame as a
whole:

``` r

pigs4 |>
  duplicate_tally()
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13        2 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.4         1
#> 4 4.22        2 7.57       1 5.92        1
#> 5 5.17        3 8.13       2 5.17        3
```

For example, `4.22` appears twice, both times in `snout`, so both of its
entries in `snout_n` are `2`. `8.13` appears once in `snout` and once in
`tail`, so both of its entries are `2` as well.

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes each `_n` column, and all of them together in the `".total"`
row:

``` r

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
```
