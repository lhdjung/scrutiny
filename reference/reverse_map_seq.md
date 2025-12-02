# Reverse the `*_map_seq()` process

`reverse_map_seq()` takes the output of a function created by
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
and reconstructs the original data frame.

See
[`audit_seq()`](https://lhdjung.github.io/scrutiny/reference/audit-special.md),
which takes `reverse_map_seq()` as a basis.

## Usage

``` r
reverse_map_seq(data)
```

## Arguments

- data:

  Data frame that inherits the `"scr_map_seq"` class.

## Value

The reconstructed tibble (data frame) which a factory-made `*_map_seq()`
function took as its `data` argument.

## Examples

``` r
# Originally reported summary data...
pigs1
#> # A tibble: 12 × 2
#>    x         n
#>    <chr> <dbl>
#>  1 7.22     32
#>  2 4.74     25
#>  3 5.23     29
#>  4 2.57     24
#>  5 6.77     27
#>  6 2.68     28
#>  7 7.01     29
#>  8 7.38     26
#>  9 3.14     27
#> 10 6.89     31
#> 11 5.00     25
#> 12 0.24     28

# ...GRIM-tested with varying inputs...
out <- grim_map_seq(pigs1, include_consistent = TRUE)

# ...and faithfully reconstructed:
reverse_map_seq(out)
#> # A tibble: 12 × 2
#>    x         n
#>    <chr> <int>
#>  1 7.22     32
#>  2 4.74     25
#>  3 5.23     29
#>  4 2.57     24
#>  5 6.77     27
#>  6 2.68     28
#>  7 7.01     29
#>  8 7.38     26
#>  9 3.14     27
#> 10 6.89     31
#> 11 5.00     25
#> 12 0.24     28
```
