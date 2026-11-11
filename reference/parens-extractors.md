# Extract substrings from before and inside parentheses

`before_parens()` and `inside_parens()` extract the part of a string
before or inside parentheses, brackets, braces, or custom delimiters.
[`split_by_parens()`](https://lhdjung.github.io/scrutiny/reference/split_by_parens.md)
does the same for the columns of a data frame.

## Usage

``` r
before_parens(string, delim = "parens", sep = deprecated())

inside_parens(string, delim = "parens", sep = deprecated())
```

## Arguments

- string:

  String vector.

- delim:

  String. The pair of delimiters around the second part: `"parens"` (the
  default), `"brackets"`, or `"braces"`, or two custom strings, matched
  literally.

- sep:

  **\[deprecated\]** The old name of `delim`.

## Value

String vector as long as `string`, with surrounding whitespace removed.
If a string has no opening delimiter, `before_parens()` returns all of
it and `inside_parens()` returns `NA`. A blank part, such as the inside
of `"()"`, is `NA`, as is a part `"NA"` or `"NaN"`.

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
