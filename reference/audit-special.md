# Summarize output of sequence mappers and total-n mappers

`audit_seq()` and `audit_total_n()` summarize the results of functions
that end on `_seq` and `_total_n`, respectively.

See below for a record of such functions. Go to the documentation of any
of them to learn about the way its output is processed by `audit_seq()`
or `audit_total_n()`.

## Usage

``` r
audit_seq(data)

audit_total_n(data)
```

## Arguments

- data:

  A data frame that inherits one of the classes named below.

## Value

A tibble (data frame) with test summary statistics.

## Details

All functions named below that end on `_seq` were made by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md).
All that end on `_total_n` were made by
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

## Before `audit_seq()`

|                                                                                        |                         |
|----------------------------------------------------------------------------------------|-------------------------|
| **Function**                                                                           | **Class**               |
| [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md)       | `"scr_grim_map_seq"`    |
| [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md) | `"scr_grimmer_map_seq"` |
| [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md)     | `"scr_debit_map_seq"`   |

## Before `audit_total_n()`

|                                                                                                |                             |
|------------------------------------------------------------------------------------------------|-----------------------------|
| **Function**                                                                                   | **Class**                   |
| [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)       | `"scr_grim_map_total_n"`    |
| [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md) | `"scr_grimmer_map_total_n"` |
| [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md)     | `"scr_debit_map_total_n"`   |

## Examples

``` r
# For GRIM-testing with dispersed inputs:
out <- pigs1 %>%
  grim_map_seq() %>%
  audit_seq()
out
#> # A tibble: 8 × 12
#>   x         n consistency hits_total hits_x hits_n diff_x diff_x_up diff_x_down
#>   <chr> <int> <lgl>            <int>  <int>  <int>  <int>     <int>       <int>
#> 1 4.74     25 FALSE                4      2      2      2         2          -2
#> 2 5.23     29 FALSE                6      3      3      1         1          -2
#> 3 2.57     24 FALSE                6      3      3      1         1          -3
#> 4 6.77     27 FALSE                7      3      4      1         1          -3
#> 5 7.01     29 FALSE                3      3      0      1         2          -1
#> 6 3.14     27 FALSE                6      3      3      1         1          -3
#> 7 6.89     31 FALSE                8      4      4      1         1          -2
#> 8 0.24     28 FALSE                6      3      3      1         1          -3
#> # ℹ 3 more variables: diff_n <int>, diff_n_up <int>, diff_n_down <int>

# Follow up on `audit_seq()` or
# `audit_total_n()` with `audit()`:
audit(out)
#> # A tibble: 9 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <dbl> <dbl> <dbl>    <dbl>   <dbl>
#> 1 hits_total   5.75 1.58     6       3     8        0   0    
#> 2 hits_x       3    0.535    3       2     4        0   0    
#> 3 hits_n       2.75 1.28     3       0     4        0   0    
#> 4 diff_x       1.12 0.354    1       1     2        0   0    
#> 5 diff_x_up    1.25 0.463    1       1     2        0   0    
#> 6 diff_x_down -2.38 0.744   -2.5    -3    -1        0   0    
#> 7 diff_n       1.43 0.787    1       1     3        1   0.125
#> 8 diff_n_up    2.29 1.38     2       1     4        1   0.125
#> 9 diff_n_down -2.57 1.40    -3      -5    -1        1   0.125
```
