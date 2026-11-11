# Count decimal places

`decimal_places()` counts the decimal places of numbers, or of numbers
written as strings.

## Usage

``` r
decimal_places(x, sep = ".")
```

## Arguments

- x:

  Numeric, or strings that contain numbers.

- sep:

  String. The separator between the integer part and the decimal places,
  matched literally. Default is `"."`. Only applies to strings; numbers
  are always written with a point.

## Value

Integer. The number of decimal places of each element of `x`.

## Details

The function counts the run of digits after the first `sep`, after
trimming whitespace and applying any exponent: `"5.30%"` has two decimal
places, `"1e-5"` has five, and `"1..5"` has none. Missing and non-finite
values have `NA` decimal places, as do the string `"NA"`, a blank
string, and strings that
[`as.numeric()`](https://rdrr.io/r/base/numeric.html) reads as an
infinity or `NaN`.

Numeric values with more than 15 significant digits cannot be counted
accurately; enter such numbers as strings.

## Trailing zeros

A numeric value has lost its trailing zeros: `3.70` is stored as `3.7`
and has one decimal place. If trailing zeros matter, count them while
the values are still strings, such as `"3.70"`, and give the counts to
the tests as `digits_x` or `digits_sd`, as in
`dplyr::mutate(data, digits_x = decimal_places(x), x = as.numeric(x))`.

## Examples

``` r
decimal_places(x = 2.851)
#> [1] 3
decimal_places(x = "2.851")
#> [1] 3

# Trailing zeros are only counted in strings:
decimal_places(x = c(7.3900, "7.3900"))
#> [1] 2 4

# Whitespace at the end of a string is ignored:
decimal_places(x = "6.0     ")
#> [1] 1
```
