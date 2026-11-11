# Test whether reported counts add up to their totals

**\[experimental\]**

`add_up_to_map()` applies
[`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md)
to every row of a data frame.

Call [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
on the output for summary statistics.

## Usage

``` r
add_up_to_map(data, summands, total = NULL, ...)
```

## Arguments

- data:

  Data frame with a `total` column and the columns to add up. Any other
  columns are returned alongside the results.

- summands:

  Columns of `data` to add up, selected with tidyselect, such as
  `starts_with("n_")`. There is no default.

- total:

  Optionally, the name of the column in `data` that contains the totals,
  if it isn't `total`.

- ...:

  These dots must be empty.

## Value

A tibble with the `summands` columns, `total`, `consistency`, and any
other columns from `data`, with the class `scrutiny_add_up_to_map`.

## Summaries with [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md) on
`add_up_to_map()` output returns a tibble with one row and these
columns:

1.  `all_cases`: total number of rows.

2.  `incon_cases`: number of inconsistent rows.

3.  `emp_incon`: proportion of inconsistent rows.

## Examples

``` r
df <- tibble::tibble(
  n_control = c(40, 25, 31),
  n_treatment = c(37, 26, 30),
  n_waitlist = c(43, 24, 30),
  total = c(120, 75, 90)
)

df |>
  add_up_to_map(summands = starts_with("n_"))
#> # A tibble: 3 × 5
#>   n_control n_treatment n_waitlist total consistency
#>       <dbl>       <dbl>      <dbl> <dbl> <lgl>      
#> 1        40          37         43   120 TRUE       
#> 2        25          26         24    75 TRUE       
#> 3        31          30         30    90 FALSE      
```
