# Print the explanations of mapper results

**\[experimental\]**

`explain_print()` prints the explanations behind the results of a mapper
such as
[`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
one row after the other. It calls
[`explain_map()`](https://lhdjung.github.io/scrutiny/reference/explain_map.md)
on the printed rows only.

## Usage

``` r
explain_print(data, ...)
```

## Arguments

- data:

  Data frame returned by
  [`grim_map()`](https://lhdjung.github.io/scrutiny/reference/grim_map.md),
  [`grimmer_map()`](https://lhdjung.github.io/scrutiny/reference/grimmer_map.md),
  or
  [`debit_map()`](https://lhdjung.github.io/scrutiny/reference/debit_map.md),
  or by their sequence and total-n mappers.

- ...:

  Optionally, the rows to print, as in
  [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html):
  positive integers to keep rows, negative ones to drop them. Default is
  to print all rows.

## Value

`data`, invisibly.

## Examples

``` r
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  explain_print(1:2)
#> GRIM test: x = 7.22, n = 32, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 7.22, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 7.215 to 7.225.
#> 
#> The mean of 32 integers is their sum divided by 32. So the sum must be from
#> 7.215 * 32 = 230.88 to 7.225 * 32 = 231.2.
#> 
#> A sum of integers is itself an integer, and 231 is in this range:
#> 231 / 32 = 7.2188, which rounds to 7.22.
#> 
#> ==> Consistent
#> 
#> GRIM test: x = 4.74, n = 25, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 4.74, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 4.735 to 4.745.
#> 
#> The mean of 25 integers is their sum divided by 25. So the sum must be from
#> 4.735 * 25 = 118.375 to 4.745 * 25 = 118.625.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 118 and 119. They give 118 / 25 = 4.72 and
#> 119 / 25 = 4.76, which round to 4.72 and 4.76, not to 4.74.
#> 
#> ==> Inconsistent
#> 

# Inconsistent rows only:
pigs1 |>
  grim_map(digits_x = 2, items_averaged = 1, data_type = "integer") |>
  dplyr::filter(!consistency) |>
  explain_print()
#> GRIM test: x = 4.74, n = 25, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 4.74, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 4.735 to 4.745.
#> 
#> The mean of 25 integers is their sum divided by 25. So the sum must be from
#> 4.735 * 25 = 118.375 to 4.745 * 25 = 118.625.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 118 and 119. They give 118 / 25 = 4.72 and
#> 119 / 25 = 4.76, which round to 4.72 and 4.76, not to 4.74.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 5.23, n = 29, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 5.23, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 5.225 to 5.235.
#> 
#> The mean of 29 integers is their sum divided by 29. So the sum must be from
#> 5.225 * 29 = 151.525 to 5.235 * 29 = 151.815.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 151 and 152. They give 151 / 29 = 5.2069 and
#> 152 / 29 = 5.2414, which round to 5.21 and 5.24, not to 5.23.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 2.57, n = 24, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 2.57, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 2.565 to 2.575.
#> 
#> The mean of 24 integers is their sum divided by 24. So the sum must be from
#> 2.565 * 24 = 61.56 to 2.575 * 24 = 61.8.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 61 and 62. They give 61 / 24 = 2.5417 and
#> 62 / 24 = 2.5833, which round to 2.54 and 2.58, not to 2.57.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 6.77, n = 27, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 6.77, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 6.765 to 6.775.
#> 
#> The mean of 27 integers is their sum divided by 27. So the sum must be from
#> 6.765 * 27 = 182.655 to 6.775 * 27 = 182.925.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 182 and 183. They give 182 / 27 = 6.7407 and
#> 183 / 27 = 6.7778, which round to 6.74 and 6.78, not to 6.77.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 7.01, n = 29, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 7.01, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 7.005 to 7.015.
#> 
#> The mean of 29 integers is their sum divided by 29. So the sum must be from
#> 7.005 * 29 = 203.145 to 7.015 * 29 = 203.435.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 203 and 204. They give 203 / 29 = 7 and 204 / 29 = 7.0345,
#> which round to 7.00 and 7.03, not to 7.01.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 3.14, n = 27, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 3.14, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 3.135 to 3.145.
#> 
#> The mean of 27 integers is their sum divided by 27. So the sum must be from
#> 3.135 * 27 = 84.645 to 3.145 * 27 = 84.915.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 84 and 85. They give 84 / 27 = 3.1111 and
#> 85 / 27 = 3.1481, which round to 3.11 and 3.15, not to 3.14.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 6.89, n = 31, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 6.89, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 6.885 to 6.895.
#> 
#> The mean of 31 integers is their sum divided by 31. So the sum must be from
#> 6.885 * 31 = 213.435 to 6.895 * 31 = 213.745.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 213 and 214. They give 213 / 31 = 6.871 and
#> 214 / 31 = 6.9032, which round to 6.87 and 6.90, not to 6.89.
#> 
#> ==> Inconsistent
#> 
#> GRIM test: x = 0.24, n = 28, items_averaged = 1, data_type = "integer"
#> 
#> The mean was reported as 0.24, with 2 decimal places. Before rounding (assuming
#> either rounding up from 5 or down from 5), it could have been anything from
#> 0.235 to 0.245.
#> 
#> The mean of 28 integers is their sum divided by 28. So the sum must be from
#> 0.235 * 28 = 6.58 to 0.245 * 28 = 6.86.
#> 
#> A sum of integers is itself an integer, but there is no integer in this range.
#> The nearest ones are 6 and 7. They give 6 / 28 = 0.2143 and 7 / 28 = 0.25,
#> which round to 0.21 and 0.25, not to 0.24.
#> 
#> ==> Inconsistent
#> 
```
