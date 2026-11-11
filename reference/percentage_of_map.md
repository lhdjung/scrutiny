# Test many reported percentages against their counts

**\[experimental\]**

`percentage_of_map()` applies
[`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
to every row of a data frame with counts, percentages, and totals.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics. See
[`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
for causes of inconsistencies.

## Usage

``` r
percentage_of_map(
  data,
  digits_percentage,
  absolute = NULL,
  percentage = NULL,
  n = NULL,
  ...,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
```

## Arguments

- data:

  Data frame with columns `absolute`, `percentage`, and `n`. Any other
  columns are returned alongside the results.

- digits_percentage:

  Integer. The number of decimal places in `percentage`, including
  trailing zeros. There is no default, since a numeric value does not
  keep them (`1.40` is `1.4`). Either a single number for the whole
  column or one number per row of `data`. If left out, it is taken from
  a `digits_percentage` column of `data`.

- absolute, percentage, n:

  Optionally, the names of the columns in `data` that contain these
  values, if they differ from the defaults.

- ...:

  These dots must be empty. Every argument after them must be named in
  full.

- rounding, threshold, symmetric:

  The same as in
  [`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

## Value

A tibble with the columns `absolute`, `percentage`, `n`,
`digits_percentage`, `consistency`, and any other columns from `data`.
It has the class `scrutiny_percentage_of_map`, which is recognized by
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md).

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`percentage_of_map()` output returns a tibble with one row and these
columns:

1.  `all_cases`: total number of value sets.

2.  `incon_cases`: number of inconsistent value sets.

3.  `emp_incon`: proportion of inconsistent value sets.

## Examples

``` r
df <- tibble::tibble(
  absolute = c(40, 37, 25),
  percentage = c(33, 31, 20),
  n = 120
)

df |>
  percentage_of_map(digits_percentage = 0)
#> # A tibble: 3 × 5
#> # Percentage: default settings
#>   absolute percentage     n digits_percentage consistency
#>      <int>      <dbl> <int>             <int> <lgl>      
#> 1       40         33   120                 0 TRUE       
#> 2       37         31   120                 0 TRUE       
#> 3       25         20   120                 0 FALSE      
```
