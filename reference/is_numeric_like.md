# Test whether a vector is numeric or coercible to numeric

**\[deprecated\]**

`is_numeric_like()` is deprecated because scrutiny no longer takes
numbers as strings. It may be removed in a future version.

`is_numeric_like()` tests whether an object contains numbers, by
scrutiny's standards:

- Integer and double vectors are `TRUE`.

- Logical vectors and non-vectors are `FALSE`.

- Other vectors, such as strings, are `TRUE` if every non-`NA` value can
  be coerced to a non-`NA` number. The strings `"NA"` and `"NaN"` count
  as `NA`, and so does a blank string, as in a blank cell of a CSV file.

- Factors, dates, and times are tested as the strings they print as.

- Lists are tested like atomic vectors if every element has length 1,
  and are `FALSE` otherwise. A logical element other than `NA` makes a
  list `FALSE`.

- A vector of nothing but non-numeric, non-logical `NA` is `NA`.

## Usage

``` r
is_numeric_like(x)
```

## Arguments

- x:

  Object to be tested.

## Value

Logical (length 1).

## Details

It asks whether a vector displays numbers, not how it is stored.

## Examples

``` r
is_numeric_like(x = 1:5)
#> Warning: `is_numeric_like()` was deprecated in scrutiny 1.0.0.
#> ℹ It may be removed in a future version of scrutiny.
#> [1] TRUE
is_numeric_like(x = c(TRUE, FALSE))
#> [1] FALSE

is_numeric_like(x = c("42", "0.7", NA))
#> [1] TRUE
is_numeric_like(x = c("42", "xyz", NA))
#> [1] FALSE

is_numeric_like(x = as.factor(c("42", "0.7", NA)))
#> [1] TRUE

is_numeric_like(x = list("42", "0.7", NA))
#> [1] TRUE
is_numeric_like(x = list("42", "0.7", NA, c(1, 2, 3)))
#> [1] FALSE

# All `NA`:
is_numeric_like(x = as.character(c(NA, NA, NA)))
#> [1] NA
is_numeric_like(x = as.numeric(c(NA, NA, NA)))
#> [1] TRUE
is_numeric_like(x = c(NA, NA, NA))
#> [1] FALSE
```
