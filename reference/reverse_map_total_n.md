# Reverse the `*_map_total_n()` process

`reverse_map_total_n()` takes the output of a function created by
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
and reconstructs the original data frame.

See
[`audit_total_n()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md),
which takes `reverse_map_total_n()` as a basis.

## Usage

``` r
reverse_map_total_n(data)
```

## Arguments

- data:

  Data frame that inherits the `"scr_map_total_n"` class.

## Value

The reconstructed tibble (data frame) which a factory-made
`*_map_total_n()` function took as its `data` argument.

## Examples

``` r
# Originally reported summary data...
df <- tibble::tribble(
  ~x1,    ~x2,   ~n,
  "3.43", "5.28", 90,
  "2.97", "4.42", 103
)
df
#> # A tibble: 2 × 3
#>   x1    x2        n
#>   <chr> <chr> <dbl>
#> 1 3.43  5.28     90
#> 2 2.97  4.42    103

# ...GRIM-tested with dispersed `n` values...
out <- grim_map_total_n(df)
out
#> # A tibble: 48 × 8
#>    x         n n_change consistency both_consistent probability  case dir  
#>    <chr> <int>    <int> <lgl>       <lgl>                 <dbl> <int> <fct>
#>  1 3.43     45        0 FALSE       FALSE                  0.55     1 forth
#>  2 5.28     45        0 FALSE       FALSE                  0.55     1 forth
#>  3 3.43     44       -1 TRUE        TRUE                   0.56     1 forth
#>  4 5.28     46        1 TRUE        TRUE                   0.54     1 forth
#>  5 3.43     43       -2 FALSE       FALSE                  0.57     1 forth
#>  6 5.28     47        2 TRUE        FALSE                  0.53     1 forth
#>  7 3.43     42       -3 TRUE        FALSE                  0.58     1 forth
#>  8 5.28     48        3 FALSE       FALSE                  0.52     1 forth
#>  9 3.43     41       -4 FALSE       FALSE                  0.59     1 forth
#> 10 5.28     49        4 FALSE       FALSE                  0.51     1 forth
#> # ℹ 38 more rows

# ...and faithfully reconstructed:
reverse_map_total_n(out)
#> # A tibble: 2 × 3
#>   x1    x2        n
#>   <chr> <chr> <dbl>
#> 1 3.43  5.28     90
#> 2 2.97  4.42    103
```
