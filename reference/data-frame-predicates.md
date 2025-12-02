# Is an object a consistency test output tibble?

- `is_map_df()` tests whether an object is the output of a
  scrutiny-style mapper function for consistency tests, like
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
  These mapper functions also include those produced by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md),
  and
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

- `is_map_basic_df()` is a variant of `is_map_df()` that tests whether
  an object is the output of a "basic" mapper function. This includes
  functions like
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md)
  and those produced by
  [`function_map()`](https://lhdjung.github.io/scrutiny/reference/function_map.md),
  but not those produced by
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
  or
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

- `is_map_seq_df()` tests whether an object is the output of a function
  that was produced by
  [`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md).

- `is_map_total_n_df()` tests whether an object is the output of a
  function that was produced by
  [`function_map_total_n()`](https://lhdjung.github.io/scrutiny/reference/function_map_total_n.md).

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

Sections 3, 6, and 7 of
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md)
discuss which function factories produce which functions, and which of
these new, factory-made functions return which kinds of tibbles.

These tibbles are what the `is_map_*()` functions test for. As an
example,
[`function_map_seq()`](https://lhdjung.github.io/scrutiny/reference/function_map_seq.md)
produces
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md),
and this new function returns a tibble. `is_map_df()` and
`is_map_seq_df()` return `TRUE` for this tibble, but `is_map_basic_df()`
and `is_map_total_n_df()` return `FALSE`.

For an overview, see the table at the end of
[`vignette("consistency-tests-in-depth")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-in-depth.md).

## Examples

``` r
# Example test output:
df1 <- grim_map(pigs1)
df2 <- grim_map_seq(pigs1)
df3 <- grim_map_total_n(tibble::tribble(
  ~x1,    ~x2,   ~n,
  "3.43", "5.28", 90,
  "2.97", "4.42", 103
))

# All three tibbles are mapper output:
is_map_df(df1)
#> [1] TRUE
is_map_df(df2)
#> [1] TRUE
is_map_df(df3)
#> [1] TRUE

# However, only `df1` is the output of a
# basic mapper...
is_map_basic_df(df1)
#> [1] TRUE
is_map_basic_df(df2)
#> [1] FALSE
is_map_basic_df(df3)
#> [1] FALSE

# ...only `df2` is the output of a
# sequence mapper...
is_map_seq_df(df1)
#> [1] FALSE
is_map_seq_df(df2)
#> [1] TRUE
is_map_seq_df(df3)
#> [1] FALSE

# ...and only `df3` is the output of a
# total-n mapper:
is_map_total_n_df(df1)
#> [1] FALSE
is_map_total_n_df(df2)
#> [1] FALSE
is_map_total_n_df(df3)
#> [1] TRUE
```
