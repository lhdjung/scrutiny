# Test whether a vector is numeric or coercible to numeric

`is_numeric_like()` tests whether an object is "coercible to numeric" by
the particular standards of scrutiny. This means:

- Integer and double vectors are `TRUE`.

- Logical vectors are `FALSE`, as are non-vector objects.

- Other vectors (most likely strings) are `TRUE` if all their non-`NA`
  values can be coerced to non-`NA` numeric values, and `FALSE`
  otherwise.

- Factors are first coerced to string, then tested.

- Lists are tested like atomic vectors unless any of their elements have
  length greater 1, in which case they are always `FALSE`.

- If all values are non-numeric, non-logical `NA`, the output is also
  `NA`.

See details for discussion.

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

The scrutiny package often deals with "number-strings", i.e., strings
that can be coerced to numeric without introducing new `NA`s. This is a
matter of displaying data in a certain way, as opposed to their storage
mode.

`is_numeric_like()` returns `FALSE` for logical vectors simply because
these are displayed as strings, not as numbers, and the usual coercion
rules would be misleading in this context. Likewise, the function treats
factors like strings because that is how they are displayed: the fact
that factors are stored as integers is irrelevant.

Why store numbers as strings or factors? Only these data types can
preserve trailing zeros, and only if the data were originally entered as
strings. See
[`vignette("wrangling")`](https://lhdjung.github.io/scrutiny/articles/wrangling.md),
section *Trailing zeros*.

## See also

The [vctrs](https://vctrs.r-lib.org/) package, which provides a serious
typing framework for R; in contrast to this rather ad-hoc function.

## Examples

``` r
# Numeric vectors are `TRUE`:
is_numeric_like(x = 1:5)
#> [1] TRUE
is_numeric_like(x = 2.47)
#> [1] TRUE

# Logical vectors are always `FALSE`:
is_numeric_like(x = c(TRUE, FALSE))
#> [1] FALSE

# Strings are `TRUE` if all of their non-`NA`
# values can be coerced to non-`NA` numbers,
# and `FALSE` otherwise:
is_numeric_like(x = c("42", "0.7", NA))
#> [1] TRUE
is_numeric_like(x = c("42", "xyz", NA))
#> [1] FALSE

# Factors are treated like their
# string equivalents:
is_numeric_like(x = as.factor(c("42", "0.7", NA)))
#> [1] TRUE
is_numeric_like(x = as.factor(c("42", "xyz", NA)))
#> [1] FALSE

# Lists behave like atomic vectors if all of their
# elements have length 1...
is_numeric_like(x = list("42", "0.7", NA))
#> [1] TRUE
is_numeric_like(x = list("42", "xyz", NA))
#> [1] FALSE

# ...but if they don't, they are `FALSE`:
is_numeric_like(x = list("42", "0.7", NA, c(1, 2, 3)))
#> [1] FALSE

# If all values are `NA`, so is the output...
is_numeric_like(x = as.character(c(NA, NA, NA)))
#> [1] NA

# ...unless the `NA`s are numeric or logical:
is_numeric_like(x = as.numeric(c(NA, NA, NA)))
#> [1] TRUE
is_numeric_like(x = c(NA, NA, NA))
#> [1] FALSE
```
