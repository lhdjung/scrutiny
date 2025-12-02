# Count duplicate values

`duplicate_count()` returns a frequency table. When searching a data
frame, it includes values from all columns for each frequency count.

This function is a blunt tool designed for initial data checking. It is
not too informative if many values have few characters each.

For summary statistics, call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
the results.

## Usage

``` r
duplicate_count(x, ignore = NULL, locations_type = c("character", "list"))
```

## Arguments

- x:

  Vector or data frame.

- ignore:

  Optionally, a vector of values that should not be counted.

- locations_type:

  String. One of `"character"` or `"list"`. With `"list"`, each
  `locations` value is a vector of column names, which is better for
  further programming. By default (`"character"`), the column names are
  pasted into a string, which is more readable.

## Value

If `x` is a data frame or another named vector, a tibble with four
columns. If `x` isn't named, only the first two columns appear:

- `value`: All the values from `x`.

- `frequency`: Absolute frequency of each value in `x`, in descending
  order.

- `locations`: Names of all columns from `x` in which `value` appears.

- `locations_n`: Number of columns named in `locations`.

The tibble has the `scr_dup_count` class, which is recognized by the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

There is an S3 method for the
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
generic, so you can call
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
following `duplicate_count()`. It returns a tibble with summary
statistics for the two numeric columns, `frequency` and `locations_n`
(or, if `x` isn't named, only for `frequency`).

## See also

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  to check each combination of columns for duplicates.

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  to show instances of a value next to each instance.

- `janitor::get_dupes()` to search for duplicate rows.

## Examples

``` r
# Count duplicate values...
iris %>%
  duplicate_count()
#> # A tibble: 77 × 4
#>    value      frequency locations                  locations_n
#>    <chr>          <int> <chr>                            <int>
#>  1 setosa            50 Species                              1
#>  2 versicolor        50 Species                              1
#>  3 virginica         50 Species                              1
#>  4 0.2               29 Petal.Width                          1
#>  5 3                 27 Sepal.Width, Petal.Length            2
#>  6 1.5               25 Petal.Length, Petal.Width            2
#>  7 1.4               21 Petal.Length, Petal.Width            2
#>  8 1.3               20 Petal.Length, Petal.Width            2
#>  9 5.1               17 Sepal.Length, Petal.Length           2
#> 10 5                 14 Sepal.Length, Petal.Length           2
#> # ℹ 67 more rows

# ...and compute summaries:
iris %>%
  duplicate_count() %>%
  audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 frequency    9.74 9.92       7     1    50        0       0
#> 2 locations_n  1.64 0.511      2     1     3        0       0

# Any values can be ignored:
iris %>%
  duplicate_count(ignore = c("setosa", "versicolor", "virginica"))
#> # A tibble: 74 × 4
#>    value frequency locations                  locations_n
#>    <chr>     <int> <chr>                            <int>
#>  1 0.2          29 Petal.Width                          1
#>  2 3            27 Sepal.Width, Petal.Length            2
#>  3 1.5          25 Petal.Length, Petal.Width            2
#>  4 1.4          21 Petal.Length, Petal.Width            2
#>  5 1.3          20 Petal.Length, Petal.Width            2
#>  6 5.1          17 Sepal.Length, Petal.Length           2
#>  7 5            14 Sepal.Length, Petal.Length           2
#>  8 2.8          14 Sepal.Width                          1
#>  9 3.2          13 Sepal.Width                          1
#> 10 5.6          12 Sepal.Length, Petal.Length           2
#> # ℹ 64 more rows
```
