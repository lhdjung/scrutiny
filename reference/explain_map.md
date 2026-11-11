# Explain mapper results row by row

**\[experimental\]**

`explain_map()` takes the output of
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
[`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
or
[`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md)
and adds an `explanation` column: the calculation behind each row's
result in plain words, as printed by
[`grim_explain()`](https://lhdjung.github.io/scrutiny/reference/grim_explain.md),
[`grimmer_explain()`](https://lhdjung.github.io/scrutiny/reference/grimmer_explain.md),
and
[`debit_explain()`](https://lhdjung.github.io/scrutiny/reference/debit_explain.md)
for a single case. It also takes the output of the sequence and total-n
mappers built on them, such as
[`grim_map_seq()`](https://lhdjung.github.io/scrutiny/reference/grim_map_seq.md).

To read the explanations, use
[`explain_print()`](https://lhdjung.github.io/scrutiny/reference/explain_print.md).

## Usage

``` r
explain_map(data, ...)
```

## Arguments

- data:

  Data frame returned by one of the mappers named above.

- ...:

  Must be empty for scrutiny's methods; methods for other classes may
  use them.

## Value

`data` with an `explanation` column, a string per row.

## Details

Each row is tested again from its own values and the settings of the
mapper call, such as `rounding` and `x_meaning`. If the result differs
from the row's `consistency` value, the row must have been changed after
testing, and `explain_map()` throws an error.

DEBIT explanations cover `formula = "exact"` only.

## Examples

``` r
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::filter(!consistency) |>
  explain_map()
#> # A tibble: 8 × 8
#> # GRIM: default settings
#>       x     n items_averaged data_type digits_x consistency exp_incon
#>   <dbl> <int>          <int> <chr>        <int> <lgl>           <dbl>
#> 1  4.74    25              1 integer          2 FALSE            0.75
#> 2  5.23    29              1 integer          2 FALSE            0.71
#> 3  2.57    24              1 integer          2 FALSE            0.72
#> 4  6.77    27              1 integer          2 FALSE            0.73
#> 5  7.01    29              1 integer          2 FALSE            0.71
#> 6  3.14    27              1 integer          2 FALSE            0.73
#> 7  6.89    31              1 integer          2 FALSE            0.69
#> 8  0.24    28              1 integer          2 FALSE            0.72
#> # ℹ 1 more variable: explanation <chr>
```
