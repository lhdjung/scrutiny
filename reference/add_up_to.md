# Do reported counts add up to their total?

**\[experimental\]**

`add_up_to()` checks whether reported subgroup sizes, or other counts,
sum to the reported total.

The function is vectorized over cases, but
[`add_up_to_map()`](https://lhdjung.github.io/scrutiny/reference/add_up_to_map.md)
is more convenient for testing many cases.

## Usage

``` r
add_up_to(summands, total)
```

## Arguments

- summands:

  Integer. The counts of one case, or a list with one such vector per
  case.

- total:

  Integer. The reported total, one per case.

## Value

Logical. `TRUE` if the counts add up to `total`, `FALSE` if not, and
`NA` if a value is missing, negative, or not a whole number, or if their
sum or the total reaches `2^53`, beyond which a sum is no longer exact.

## Examples

``` r
add_up_to(c(40, 37, 43), total = 120)
#> [1] TRUE
add_up_to(c(40, 37, 44), total = 120)
#> [1] FALSE

# Several cases:
add_up_to(list(c(40, 37, 43), c(10, 12)), total = c(120, 23))
#> [1]  TRUE FALSE
```
