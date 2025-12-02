# Rank sequence test results

Run this function after generating a sequence with
[`seq_endpoint_df()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
or
[`seq_distance_df()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
and testing it with one of scrutiny's mapping functions, such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md).
It will rank the test's consistent and inconsistent results by their
positions in the sequence.

## Usage

``` r
seq_test_ranking(x, explain = TRUE)
```

## Arguments

- x:

  Data frame.

- explain:

  If `TRUE` (the default), results come with an explanation.

## Value

A tibble (data frame). The function will also print an explanation of
the results. See examples.

## Details

The function checks the provenance of the test results and throws a
warning if it's not correct.

## Examples

``` r
seq_distance_df(.from = "0.00", n = 50) %>%
  grim_map() %>%
  seq_test_ranking()
#> Explanation:
#> ℹ There are 5 consistent value sets, starting with row number 1 in the data
#>   frame created by `grim_map()`.
#> ℹ All other value sets are inconsistent.
#> ℹ The consistent sets lead the inconsistent ones by numbers of places from 1 to
#>   1 in the `grim_map()` data frame.
#> # A tibble: 5 × 3
#>   consistent inconsistent lead_lag
#>        <int>        <int>    <int>
#> 1          1            2        1
#> 2          3            4        1
#> 3          5            6        1
#> 4          7            8        1
#> 5          9           10        1
```
