# Reconstruct the input of a total-n mapper

`reverse_map_total_n()` takes the output of a function made by
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
and reconstructs the reported columns of the data frame it was called
on. [`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
builds on it for total-n output.

## Usage

``` r
reverse_map_total_n(data)
```

## Arguments

- data:

  Data frame with the `"scrutiny_map_total_n"` class.

## Value

A tibble with the reported columns, such as `x1`, `x2`, and `n`, one row
per case.

## Examples

``` r
df <- tibble::tribble(
  ~x1,  ~x2,  ~n,
  3.43, 5.28, 90,
  2.97, 4.42, 103
)

out <- grim_map_total_n(
  df,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)

reverse_map_total_n(out)
#> # A tibble: 2 × 3
#>      x1    x2     n
#>   <dbl> <dbl> <int>
#> 1  3.43  5.28    90
#> 2  2.97  4.42   103
```
