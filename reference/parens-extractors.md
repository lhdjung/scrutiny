# Extract substrings from before and inside parentheses

`before_parens()` and `inside_parens()` extract substrings from before
or inside parentheses, or similar separators like brackets or curly
braces.

See
[`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
to split some or all columns in a data frame into both parts.

## Usage

``` r
before_parens(string, sep = "parens")

inside_parens(string, sep = "parens")
```

## Arguments

- string:

  Vector of strings with parentheses or similar.

- sep:

  String. What to split by. Either `"parens"`, `"brackets"`, `"braces"`,
  or a length-2 vector of custom separators. See examples for
  [`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md).
  Default is `"parens"`.

## Value

String vector of the same length as `string`. The part of `string`
before or inside `sep`, respectively.

## Examples

``` r
x <- c(
  "3.72 (0.95)",
  "5.86 (2.75)",
  "3.06 (6.48)"
)

before_parens(string = x)
#> [1] "3.72" "5.86" "3.06"

inside_parens(string = x)
#> [1] "0.95" "2.75" "6.48"
```
