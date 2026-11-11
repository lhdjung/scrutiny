# Vary hypothetical group sizes

If a study reports a total sample size but no group sizes, these
functions generate pairs of hypothetical group sizes that add up to the
total:

- `disperse()` takes half of an even total and goes up and down from it.

- `disperse2()` takes the two consecutive numbers around half of an odd
  total, and goes down from the lower and up from the upper.

- `disperse_total()` takes the total itself and calls one of the two,
  depending on whether it is even or odd.

They are the basis of
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
and the other functions made by
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

## Usage

``` r
disperse(n, dispersion = 0:5, n_min = 1L, n_max = NULL, constant = NULL)

disperse2(n, dispersion = 0:5, n_min = 1L, n_max = NULL, constant = NULL)

disperse_total(n, dispersion = 0:5, n_min = 1L, n_max = NULL, constant = NULL)
```

## Arguments

- n:

  Numeric. In `disperse()`, half of an even total. In `disperse2()`, the
  two consecutive numbers closest to half of an odd total, such as
  `c(25, 26)` for 51. In `disperse_total()`, the total itself.

- dispersion:

  Numeric. Non-negative whole numbers of steps up and down from `n` (or,
  in `disperse_total()`, from half of `n`). Default is `0:5`.

- n_min:

  Numeric. Minimal group size. Default is `1L`.

- n_max:

  Numeric. Maximal group size. Default is `NULL`, no maximum.

- constant:

  Optionally, a length-2 vector, or a list of such vectors (which
  includes a data frame with two rows), to repeat alongside each pair of
  group sizes. Default is `NULL`.

## Value

A tibble with these columns:

- `n`: the group sizes. Each pair of consecutive rows adds up to the
  total.

- `n_change`: the number of steps from the input `n`, negative for the
  lower group size of each pair.

## Details

A pair with a group size below `n_min` or above `n_max` is dropped.
Columns from `constant` are named after a named list's elements, or
`"constant1"`, `"constant2"`, etc. for an unnamed one; a single pair is
`"constant"`.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md)
and
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md).

## Examples

``` r
# For a total of 40, `n` is 20:
disperse(n = 20)
#> # A tibble: 12 × 2
#>        n n_change
#>    <int>    <int>
#>  1    20        0
#>  2    20        0
#>  3    19       -1
#>  4    21        1
#>  5    18       -2
#>  6    22        2
#>  7    17       -3
#>  8    23        3
#>  9    16       -4
#> 10    24        4
#> 11    15       -5
#> 12    25        5

disperse(n = 20, dispersion = c(3, 6, 10))
#> # A tibble: 6 × 2
#>       n n_change
#>   <int>    <int>
#> 1    17       -3
#> 2    23        3
#> 3    14       -6
#> 4    26        6
#> 5    10      -10
#> 6    30       10

# For an odd total, `disperse2()` takes two group sizes:
disperse2(n = c(25, 26))
#> # A tibble: 12 × 2
#>        n n_change
#>    <int>    <int>
#>  1    25        0
#>  2    26        0
#>  3    24       -1
#>  4    27        1
#>  5    23       -2
#>  6    28        2
#>  7    22       -3
#>  8    29        3
#>  9    21       -4
#> 10    30        4
#> 11    20       -5
#> 12    31        5

# `disperse_total()` takes the total itself:
disperse_total(n = 40)
#> # A tibble: 12 × 2
#>        n n_change
#>    <int>    <int>
#>  1    20        0
#>  2    20        0
#>  3    19       -1
#>  4    21        1
#>  5    18       -2
#>  6    22        2
#>  7    17       -3
#>  8    23        3
#>  9    16       -4
#> 10    24        4
#> 11    15       -5
#> 12    25        5
disperse_total(n = 51)
#> # A tibble: 12 × 2
#>        n n_change
#>    <int>    <int>
#>  1    25        0
#>  2    26        0
#>  3    24       -1
#>  4    27        1
#>  5    23       -2
#>  6    28        2
#>  7    22       -3
#>  8    29        3
#>  9    21       -4
#> 10    30        4
#> 11    20       -5
#> 12    31        5

# Values that repeat alongside the pairs:
disperse_total(37, constant = c("5.24", "3.80"))
#> # A tibble: 12 × 3
#>        n n_change constant
#>    <int>    <int> <chr>   
#>  1    18        0 5.24    
#>  2    19        0 3.80    
#>  3    17       -1 5.24    
#>  4    20        1 3.80    
#>  5    16       -2 5.24    
#>  6    21        2 3.80    
#>  7    15       -3 5.24    
#>  8    22        3 3.80    
#>  9    14       -4 5.24    
#> 10    23        4 3.80    
#> 11    13       -5 5.24    
#> 12    24        5 3.80    

df_constant <- tibble::tibble(
  name = c("Paul", "Mathilda"), age = 27:28,
  registered = c(TRUE, FALSE)
)
disperse_total(37, constant = df_constant)
#> # A tibble: 12 × 5
#>        n n_change name       age registered
#>    <int>    <int> <chr>    <int> <lgl>     
#>  1    18        0 Paul        27 TRUE      
#>  2    19        0 Mathilda    28 FALSE     
#>  3    17       -1 Paul        27 TRUE      
#>  4    20        1 Mathilda    28 FALSE     
#>  5    16       -2 Paul        27 TRUE      
#>  6    21        2 Mathilda    28 FALSE     
#>  7    15       -3 Paul        27 TRUE      
#>  8    22        3 Mathilda    28 FALSE     
#>  9    14       -4 Paul        27 TRUE      
#> 10    23        4 Mathilda    28 FALSE     
#> 11    13       -5 Paul        27 TRUE      
#> 12    24        5 Mathilda    28 FALSE     
```
