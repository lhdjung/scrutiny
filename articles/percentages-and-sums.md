# Percentages and sums

Two tests check whether reported percentages and sums can have been
derived from reported counts:

- [`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
  and
  [`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
  check whether a reported percentage is consistent with a reported
  count and total.
- [`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md)
  and
  [`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
  check whether reported counts, such as group sizes, sum to a reported
  total.

The math is trivial. What the functions add is safe application at
scale: the `*_map()` functions test every row of a data frame,
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes their results, and
[`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
handles rounding in exact integer arithmetic, so floating-point error
cannot change a result. All of these functions are experimental.

## Percentages

### Single cases: `percentage_of()`

40 out of 120 is 33.33…%, so it can be reported as 33 or 33.3, but not
as 34:

``` r

percentage_of(absolute = 40, percentage = 33, n = 120, digits_percentage = 0)
#> [1] TRUE
percentage_of(40, 34, 120, digits_percentage = 0)
#> [1] FALSE

percentage_of(
  absolute = 40,
  percentage = 33.3,
  n = 120,
  digits_percentage = 1
)
#> [1] TRUE
```

`digits_percentage` is required, like `digits_x` in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md), since
a numeric value does not keep trailing zeros. `rounding`, `threshold`,
and `symmetric` work as in
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md).

This check is stricter than
[`grim()`](https://lhdjung.github.io/scrutiny/reference/grim.md) with
`x_meaning = "percentage"`, which only asks whether *some* count out of
`n` could have produced the percentage.
[`percentage_of()`](https://lhdjung.github.io/scrutiny/reference/percentage_of.md)
asks about one specific count, so it needs that count to be reported.

### Many cases: `percentage_of_map()`

[`percentage_of_map()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map.md)
tests every row of a data frame with the columns `absolute`,
`percentage`, and `n`:

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

25 out of 120 is 20.8%, which would have been reported as 21, so the
third row is inconsistent.
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes the results:

``` r

df |>
  percentage_of_map(digits_percentage = 0) |>
  audit()
#> # A tibble: 1 × 3
#>   all_cases incon_cases emp_incon
#>       <int>       <int>     <dbl>
#> 1         3           1     0.333
```

### Investigating possible causes of inconsistencies

A `FALSE` result means that the three values cannot all be correct as
reported, not why. The cause might be trivial: the percentage may be
based on a denominator other than `n`, such as the number of valid
responses or the sample size before loss to follow-up.

[`percentage_of_map_seq()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map_seq.md)
varies the counts, percentages, and totals a few steps up and down and
tests each resulting value set, as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)
does. With `var = "n"`, it shows which nearby denominators would make
the percentage consistent:

``` r

df |>
  percentage_of_map_seq(digits_percentage = 0, var = "n")
#> # A tibble: 10 × 8
#> # Percentage: var = "n"
#>    absolute percentage     n digits_percentage consistency diff_var  case var  
#>       <int>      <dbl> <int>             <int> <lgl>          <int> <int> <chr>
#>  1       25         20   115                 0 FALSE             -5     3 n    
#>  2       25         20   116                 0 FALSE             -4     3 n    
#>  3       25         20   117                 0 FALSE             -3     3 n    
#>  4       25         20   118                 0 FALSE             -2     3 n    
#>  5       25         20   119                 0 FALSE             -1     3 n    
#>  6       25         20   121                 0 FALSE              1     3 n    
#>  7       25         20   122                 0 TRUE               2     3 n    
#>  8       25         20   123                 0 TRUE               3     3 n    
#>  9       25         20   124                 0 TRUE               4     3 n    
#> 10       25         20   125                 0 TRUE               5     3 n
```

The nearest consistent total is two steps away: 25 out of 122 is 20.49%,
reported as 20.

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
counts the “hits”, the varied value sets that are consistent:

``` r

df |>
  percentage_of_map_seq(digits_percentage = 0, var = "n") |>
  audit()
#> # A tibble: 1 × 9
#>   absolute percentage     n consistency hits_total hits_n diff_n diff_n_up
#>      <int>      <dbl> <int> <lgl>            <int>  <int>  <int>     <int>
#> 1       25         20   120 FALSE                4      4      2         2
#> # ℹ 1 more variable: diff_n_down <int>
```

## Sums

### Single cases: `add_up_to()`

[`add_up_to()`](https://lhdjung.github.io/scrutiny/reference/add_up_to.md)
checks whether counts sum to a total:

``` r

add_up_to(c(40, 37, 43), total = 120)
#> [1] TRUE
add_up_to(c(40, 37, 44), total = 120)
#> [1] FALSE
```

A missing or fractional value gives `NA`: a fractional count describes
no group, and a sum of decimals is not exact in floating-point
arithmetic.

### Many cases: `add_up_to_map()`

[`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
adds up any number of columns, selected with [tidyselect
syntax](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) as
in
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
and compares the sum to a `total` column:

``` r

df_groups <- tibble::tribble(
  ~n_control , ~n_treatment , ~n_waitlist , ~total ,
          40 ,           37 ,          43 ,    120 ,
          25 ,           26 ,          24 ,     75 ,
          31 ,           30 ,          30 ,     90 ,
)

df_groups |>
  add_up_to_map(summands = starts_with("n_"))
#> # A tibble: 3 × 5
#>   n_control n_treatment n_waitlist total consistency
#>       <dbl>       <dbl>      <dbl> <dbl> <lgl>      
#> 1        40          37         43   120 TRUE       
#> 2        25          26         24    75 TRUE       
#> 3        31          30         30    90 FALSE

df_groups |>
  add_up_to_map(summands = starts_with("n_")) |>
  audit()
#> # A tibble: 1 × 3
#>   all_cases incon_cases emp_incon
#>       <int>       <int>     <dbl>
#> 1         3           1     0.333
```
