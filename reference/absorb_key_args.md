# Rename key columns according to the key arguments

**\[superseded\]**

`absorb_key_args()` is superseded by
[`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
whose mappers rename key columns themselves.

Call it in a handwritten mapper. If a key argument such as `x` names a
column of `data`, it renames that column to the key argument's name, so
that the rest of the mapper finds it.

## Usage

``` r
absorb_key_args(data, reported, key_cols_call = NULL)
```

## Arguments

- data:

  Data frame passed to the mapper.

- reported:

  String. Names of the key arguments.

- key_cols_call:

  Optionally, a named character vector that maps key argument names to
  the columns of `data` that they refer to, such as `c(x = "mean")`. By
  default, it is read from the key arguments of the calling mapper.

## Value

`data`, possibly with columns renamed. Reassign the result to `data`.

## Examples

``` r
df <- tibble::tibble(mean = c(5.19, 4.56), n = c(28, 30))
absorb_key_args(df, c("x", "n"), key_cols_call = c(x = "mean"))
#> # A tibble: 2 × 2
#>       x     n
#>   <dbl> <dbl>
#> 1  5.19    28
#> 2  4.56    30
```
