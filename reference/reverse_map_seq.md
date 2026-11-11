# Reconstruct the input of a sequence mapper

`reverse_map_seq()` takes the output of a function made by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
and reconstructs the tested columns of the cases it dispersed, without
`digits_*`.
[`audit()`](https://lhdjung.github.io/scrutiny/reference/audit.md)
builds on it for sequence output.

## Usage

``` r
reverse_map_seq(data)
```

## Arguments

- data:

  Data frame with the `"scrutiny_map_seq"` class.

## Value

A tibble with the tested columns, one row per case in `data`.

## Examples

``` r
pigs1
#> # A tibble: 12 × 2
#>        x     n
#>    <dbl> <dbl>
#>  1  7.22    32
#>  2  4.74    25
#>  3  5.23    29
#>  4  2.57    24
#>  5  6.77    27
#>  6  2.68    28
#>  7  7.01    29
#>  8  7.38    26
#>  9  3.14    27
#> 10  6.89    31
#> 11  5       25
#> 12  0.24    28

out <- grim_map_seq(
  pigs1,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer",
  include_consistent = TRUE
)

reverse_map_seq(out)
#> # A tibble: 12 × 4
#>        x     n items_averaged data_type
#>    <dbl> <int>          <int> <chr>    
#>  1  7.22    32              1 integer  
#>  2  4.74    25              1 integer  
#>  3  5.23    29              1 integer  
#>  4  2.57    24              1 integer  
#>  5  6.77    27              1 integer  
#>  6  2.68    28              1 integer  
#>  7  7.01    29              1 integer  
#>  8  7.38    26              1 integer  
#>  9  3.14    27              1 integer  
#> 10  6.89    31              1 integer  
#> 11  5       25              1 integer  
#> 12  0.24    28              1 integer  
```
