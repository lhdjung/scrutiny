# Vary hypothetical group sizes

Some published studies only report a total sample size but no group
sizes. However, group sizes are crucial for consistency tests such as
GRIM. Call `disperse()` to generate possible group sizes that all add up
to the total sample size, if that total is even.

`disperse2()` is a variant for odd totals. It takes two consecutive
numbers and generates decreasing values from the lower as well as
increasing values from the upper. In this way, all combinations still
add up to the total.

`disperse_total()` directly takes the total sample size, checks if it's
even or odd, splits it up accordingly, and applies `disperse()` or
`disperse2()`, respectively.

These functions are primarily intended as helpers. They form the
backbone of
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md)
and all other functions created with
[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

## Usage

``` r
disperse(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL,
  constant_index = NULL
)

disperse2(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL,
  constant_index = NULL
)

disperse_total(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL,
  constant_index = NULL
)
```

## Arguments

- n:

  Numeric:

  - In `disperse()`, single number from which to go up and down. This
    should be half of an even total sample size.

  - In `disperse2()`, the two consecutive numbers closest to half of an
    odd total sample size (e.g., `c(25, 26)` for a total of 51).

  - In `disperse_total()`, the total sample size.

- dispersion:

  Numeric. Vector that determines the steps up and down from `n` (or, in
  `disperse_total()`, from half `n`). Default is `0:5`.

- n_min:

  Numeric. Minimal group size. Default is `1L`.

- n_max:

  Numeric. Maximal group size. Default is `NULL`, i.e., no maximum.

- constant:

  Optionally, add a length-2 vector or a list of length-2 vectors (such
  as a data frame with exactly two rows) to accompany the pairs of
  dispersed values. Default is `NULL`, i.e., no constant values.

- constant_index:

  Integer (length 1). Index of `constant` or the first `constant` column
  in the output tibble. If `NULL` (the default), `constant` will go to
  the right of `n_change`.

## Value

A tibble (data frame) with these columns:

- `n` includes the dispersed `n` values. Every pair of consecutive rows
  has `n` values that each add up to the total.

- `n_change` records how the input `n` was transformed to the output
  `n`. In `disperse2()`, the `n_change` strings label the lower of the
  input `n` values `n1` and the higher one `n2`.

## Details

If any group size is less than `n_min` or greater than `n_max`, it is
removed. The complementary size of the other group is also removed.

`constant` values are pairwise repeated. That is why `constant` must be
a length-2 atomic vector or a list of such vectors. If `constant` is a
data frame or some other named list, the resulting columns will have the
same names as the list-element names. If the list is not named, the new
column names will be `"constant1"`, `"constant2"`, etc; or just
`"constant"`, for a single pair.

## References

Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It Light
or Dark? Recalling Moral Behavior Changes Perception of Brightness.
*Psychological Science*, 32(12), 2042–2043.
https://journals.sagepub.com/doi/10.1177/09567976211058727

## See also

[`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
[`grim_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/grim_map_total_n.md),
and
[`seq_distance_df()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md).

## Examples

``` r
# For a total sample size of 40,
# set `n` to `20`:
disperse(n = 20)
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
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

# Specify `dispersion` to control
# the steps up and down from `n`:
disperse(n = 20, dispersion = c(3, 6, 10))
#> # A tibble: 6 × 2
#>       n n_change
#>   <dbl>    <int>
#> 1    17       -3
#> 2    23        3
#> 3    14       -6
#> 4    26        6
#> 5    10      -10
#> 6    30       10

# In `disperse2()`, specify `n` as two
# consecutive numbers -- i.e., group sizes:
disperse2(n = c(25, 26))
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
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

# Use the total sample size directly
# with `disperse_total()`. An even total
# internally triggers `disperse()`...
disperse_total(n = 40)
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
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

# ...whereas an odd total triggers `disperse2()`:
disperse_total(n = 51)
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
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

# You may add values that repeat along with the
# dispersed ones but remain constant themselves.
# Such values can be stored in a length-2 vector
# for a single column...
disperse_total(37, constant = c("5.24", "3.80"))
#> # A tibble: 12 × 3
#>        n n_change constant
#>    <dbl>    <int> <chr>   
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

# ... or a list of length-2 vectors for multiple
# columns. This includes data frames with 2 rows:
df_constant <- tibble::tibble(
  name = c("Paul", "Mathilda"), age = 27:28,
  registered = c(TRUE, FALSE)
)
disperse_total(37, constant = df_constant)
#> # A tibble: 12 × 5
#>        n n_change name       age registered
#>    <dbl>    <int> <chr>    <int> <lgl>     
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
