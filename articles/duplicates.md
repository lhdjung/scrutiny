# Duplication analysis

``` r
library(scrutiny)
```

You can use scrutiny to detect duplicate values in any dataset.
Duplicates can go a long way in assessing the reliability of published
research.

This vignette walks you through scrutiny’s tools for detecting,
counting, and summarizing duplicates. It uses the `pigs4` dataset as a
simple example:

``` r
pigs4
#> # A tibble: 5 × 3
#>   snout tail  wings
#>   <chr> <chr> <chr>
#> 1 4.73  6.88  6.09 
#> 2 8.13  7.33  8.27 
#> 3 4.22  5.17  4.40 
#> 4 4.22  7.57  5.92 
#> 5 5.17  8.13  5.17
```

## Frequency tabulation with `duplicate_count()`

A good first step is to create a frequency table. To do so, use
[`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md):

``` r
pigs4 %>% 
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
#>  8 4.40          1 wings                        1
#>  9 5.92          1 wings                        1
#> 10 6.09          1 wings                        1
#> 11 8.27          1 wings                        1
```

It returns a tibble (data frame) that lists each unique `value`. The
tibble is ordered by the `frequency` of values in the input data frame,
so the values that appear most often are at the top. The `locations` are
the names of all the columns in which a given value appears. They are
counted by `locations_n`.

For example, `5.17` is the most frequent value in `pigs4`. It appears 3
times (`frequency`), namely in the `snout`, `tail`, and `wings` columns;
so `locations_n` is also `3`. The next most frequent value is `4.22`
which appears twice, but both of these instances are in the `snout`
column, so `locations_n` is `1`.

Run [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
after
[`duplicate_count()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count.md)
to get summary statistics for the two numeric columns, `frequency` and
`locations_n`:

``` r
pigs4 %>% 
    duplicate_count() %>% 
    audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 frequency    1.36 0.674      1     1     3        0       0
#> 2 locations_n  1.27 0.647      1     1     3        0       0
```

## Counting by column pair with `duplicate_count_colpair()`

Sometimes, a sequence of data may be repeated in multiple columns.
[`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
helps find such cases:

``` r
pigs4 %>% 
  duplicate_count_colpair()
#> # A tibble: 3 × 7
#>   x     y     count total_x total_y rate_x rate_y
#>   <chr> <chr> <int>   <int>   <int>  <dbl>  <dbl>
#> 1 snout tail      2       5       5    0.4    0.4
#> 2 snout wings     1       5       5    0.2    0.2
#> 3 tail  wings     1       5       5    0.2    0.2
```

`x` and `y` represent all combinations of columns in `pigs4`. The
`count` is the number of values that appear in both respective columns.
`total_x` and `total_y` are the numbers of non-missing values in the
original columns listed under `x` and `y`. Similarly, `rate_x` is the
rate of `x` values that also appear in `y`, and `rate_y` is the rate of
`y` values that also appear in `x`. If there are no missing values,
`total_x` is the same as `total_y`, and `rate_x` is the same as
`rate_y`.

Here, `snout` and `tail` are the column pair with the most overlap: 2
out of 5 values are the same, a duplication rate of 0.4.

Again, you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) for
summary statistics:

``` r
pigs4 %>% 
  duplicate_count_colpair() %>% 
  audit()
#> # A tibble: 5 × 8
#>   term     mean    sd median   min   max na_count na_rate
#>   <chr>   <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 count   1.33  0.577    1     1     2          0       0
#> 2 total_x 5     0        5     5     5          0       0
#> 3 total_y 5     0        5     5     5          0       0
#> 4 rate_x  0.267 0.115    0.2   0.2   0.4        0       0
#> 5 rate_y  0.267 0.115    0.2   0.2   0.4        0       0
```

## Counting by observation with `duplicate_tally()`

Unlike the other two functions,
[`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
largely preserves the structure of the original data frame. It only adds
a column ending on `_n` next to each original column. The new columns
count how often the values to their left appear in the data frame as a
whole:

``` r
pigs4 %>% 
    duplicate_tally()
#> # A tibble: 5 × 6
#>   snout snout_n tail  tail_n wings wings_n
#>   <chr>   <int> <chr>  <int> <chr>   <int>
#> 1 4.73        1 6.88       1 6.09        1
#> 2 8.13        2 7.33       1 8.27        1
#> 3 4.22        2 5.17       3 4.40        1
#> 4 4.22        2 7.57       1 5.92        1
#> 5 5.17        3 8.13       2 5.17        3
```

For example, `4.22` appears twice in `snout` but not in any other
column, so the corresponding entries in `snout_n` are `2`. Compare this
to `8.13`, which appears once in `snout` and once in `tail` (but not in
`wings`), so both observations are marked `2` in the `_n` columns.

When following up
[`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md),
it shows summary statistics for each `_n` column. The last row
summarizes all of these columns together, with `"term"` saying
`".total"`:

``` r
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
```
