# Classes of sequence and total-n mapper output

[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes the output of functions made by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
and of those made by
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
such as
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md),
with one row per case. The columns of each summary are described in the
documentation of the respective mapper.

Such output carries the generic class `"scrutiny_map_seq"` or
`"scrutiny_map_total_n"`, which
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
dispatches on, and a class for the particular mapper, listed below, so
that you can write your own S3 methods for it. The summaries have the
class `"scrutiny_audit_seq"` or `"scrutiny_audit_total_n"`, which
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
summarizes further.

## Sequence mappers

|  |  |
|----|----|
| **Function** | **Class** |
| [`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md) | `"scrutiny_grim_map_seq"` |
| [`grimmer_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_seq.md) | `"scrutiny_grimmer_map_seq"` |
| [`debit_map_seq()`](https://lhdjung.github.io/scrutiny/reference/debit_map_seq.md) | `"scrutiny_debit_map_seq"` |
| [`percentage_of_map_seq()`](https://lhdjung.github.io/scrutiny/reference/percentage_of_map_seq.md) | `"scrutiny_percentage_of_map_seq"` |

## Total-n mappers

|  |  |
|----|----|
| **Function** | **Class** |
| [`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md) | `"scrutiny_grim_map_total_n"` |
| [`grimmer_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map_total_n.md) | `"scrutiny_grimmer_map_total_n"` |
| [`debit_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/debit_map_total_n.md) | `"scrutiny_debit_map_total_n"` |

## Examples

``` r
out <- pigs1 |>
  grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  audit()
out
#> # A tibble: 8 × 14
#>       x     n items_averaged data_type consistency hits_total hits_x hits_n
#>   <dbl> <int>          <int> <chr>     <lgl>            <int>  <int>  <int>
#> 1  4.74    25              1 integer   FALSE                4      2      2
#> 2  5.23    29              1 integer   FALSE                6      3      3
#> 3  2.57    24              1 integer   FALSE                6      3      3
#> 4  6.77    27              1 integer   FALSE                7      3      4
#> 5  7.01    29              1 integer   FALSE                3      3      0
#> 6  3.14    27              1 integer   FALSE                6      3      3
#> 7  6.89    31              1 integer   FALSE                8      4      4
#> 8  0.24    28              1 integer   FALSE                6      3      3
#> # ℹ 6 more variables: diff_x <int>, diff_x_up <int>, diff_x_down <int>,
#> #   diff_n <int>, diff_n_up <int>, diff_n_down <int>

# `audit()` summarizes the summary:
audit(out)
#> # A tibble: 9 × 8
#>   term         mean    sd median   min   max na_count na_rate
#>   <chr>       <dbl> <dbl>  <dbl> <int> <int>    <int>   <dbl>
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
