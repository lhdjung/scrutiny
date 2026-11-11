# Is an object the output of a mapper function?

- `is_map_df()` tests whether an object is the output of a mapper of any
  tier, i.e., of a function made by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  or
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md),
  such as
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).

- `is_map_basic_df()`, `is_map_seq_df()`, and `is_map_total_n_df()` test
  for one tier each.

## Usage

``` r
is_map_df(x)

is_map_basic_df(x)

is_map_seq_df(x)

is_map_total_n_df(x)
```

## Arguments

- x:

  Object to be tested.

## Value

Logical (length 1).

## Details

See
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md)
for the three tiers and the table at its end.

## Examples

``` r
df1 <- pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer")
df2 <- pigs1 |>
  grim_map_seq(digits_x = 2, items_averaged = 1, data_type = "integer")
df3 <- tibble::tribble(
  ~x1,  ~x2,  ~n,
  3.43, 5.28, 90,
  2.97, 4.42, 103
) |>
  grim_map_total_n(digits_x = 2, items_averaged = 1, data_type = "integer")

# All three are mapper output:
is_map_df(df1)
#> [1] TRUE
is_map_df(df2)
#> [1] TRUE
is_map_df(df3)
#> [1] TRUE

# Each is the output of one tier:
is_map_basic_df(df1)
#> [1] TRUE
is_map_seq_df(df2)
#> [1] TRUE
is_map_total_n_df(df3)
#> [1] TRUE

is_map_basic_df(df2)
#> [1] FALSE
is_map_seq_df(df3)
#> [1] FALSE
```
