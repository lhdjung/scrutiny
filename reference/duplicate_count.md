# Count duplicate values

`duplicate_count()` returns a frequency table of the values in a vector
or data frame. For a data frame, it pools the values of all columns and
records which columns each value appears in.

This is a blunt tool for initial checks. If many values have few
characters each, duplicates occur by chance and mean little.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
duplicate_count(x, ignore = NULL, locations_type = c("character", "list"))
```

## Arguments

- x:

  Vector, matrix, or data frame. Missing values are not counted, nor are
  blank strings, `"NA"`, and `"NaN"`, which count as missing.

- ignore:

  Optionally, a vector of values not to count.

- locations_type:

  String. `"character"` (the default) pastes the column names in
  `locations` into one string; `"list"` keeps them as a vector per
  value, which is better for programming.

## Value

A tibble with these columns, or only the first two if `x` is not named:

- `value`: the distinct values of `x`.

- `frequency`: how often each occurs, in descending order.

- `locations`: the names of the columns of `x` in which `value` appears.

- `locations_n`: the number of such columns.

The tibble has the class `scrutiny_dup_count`, which is recognized by
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`duplicate_count()` output returns summary statistics for `frequency`
and `locations_n` (or `frequency` alone if `x` is not named).

## See also

- [`duplicate_count_colpair()`](https://lhdjung.github.io/scrutiny/reference/duplicate_count_colpair.md)
  for duplicates between pairs of columns.

- [`duplicate_tally()`](https://lhdjung.github.io/scrutiny/reference/duplicate_tally.md)
  for counts next to each value.

- `janitor::get_dupes()` for duplicate rows.

## Examples

``` r
iris |>
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

iris |>
  duplicate_count() |>
  audit()
#> # A tibble: 2 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <int> <int> <int>    <int>   <dbl>
#> 1 frequency    9.74 9.92       7     1    50        0       0
#> 2 locations_n  1.64 0.511      2     1     3        0       0

# Ignore some values:
iris |>
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
