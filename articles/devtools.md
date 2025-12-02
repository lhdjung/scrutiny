# Developer tools

``` r
library(scrutiny)
```

The implementation of error detection techniques in scrutiny rests on a
foundation of specialized helper functions. Some of these are exported
because they might be helpful in error detection more broadly, or
perhaps even in other contexts.

This vignette provides an overview of scrutiny’s miscellaneous
infrastructure for implementing error detection techniques. For more
specific articles, see
[`vignette("rounding-in-depth")`](https://lhdjung.github.io/scrutiny/articles/rounding-in-depth.md)
or
[`vignette("consistency-tests-simple")`](https://lhdjung.github.io/scrutiny/articles/consistency-tests-simple.md).

### Count decimal places

Large parts of the package ultimately rest on either of two functions
that simply count decimal places. These are digits after a number’s
decimal point or some other separator. Both functions also take strings.

[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
is vectorized:

``` r
decimal_places("2.80")
#> [1] 2

decimal_places(c(55.1, 6.493, 8))
#> [1] 1 3 0

vec1 <- iris %>% 
  dplyr::slice(1:10) %>% 
  dplyr::pull(Sepal.Length)

vec1
#>  [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9

vec1 %>% 
  decimal_places()
#>  [1] 1 1 1 1 0 1 1 0 1 1
```

Using strings (that are coercible to numeric) is recommended in an error
detection context because trailing zeros can be crucial here. Numeric
values drop trailing zeros, whereas strings preserve them:

``` r
decimal_places(7.200)
#> [1] 1

decimal_places("7.200")
#> [1] 3
```

[`decimal_places_scalar()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
is faster than
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
but only takes a single number or string. This makes it suitable as a
helper within other single-case functions.

### Restore trailing zeros

When dealing with numbers that used to have trailing zeros but lost them
from being registered as numeric, call
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md)
to format them correctly. This can be relevant within functions that
create vectors where trailing zeros matter, such as the `seq_*()`
functions presented in the next section.

Suppose all of the following numbers originally had one decimal place,
but some no longer do:

``` r
vec2 <- c(4, 6.9, 5, 4.2, 4.8, 7, 4)

vec2 %>% 
  decimal_places()
#> [1] 0 1 0 1 1 0 0
```

Now, get them back with
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md):

``` r
vec2 %>% 
  restore_zeros()
#> [1] "4.0" "6.9" "5.0" "4.2" "4.8" "7.0" "4.0"

vec2 %>% 
  restore_zeros() %>% 
  decimal_places()
#> [1] 1 1 1 1 1 1 1
```

This uses the default of going by the longest mantissa and padding the
other strings with decimal zeros until they have that many decimal
places. However, this is just a heuristic: The longest mantissa might
itself have lost decimal places. Specify the `width` argument to
explicitly state the desired mantissa length:

``` r
vec2 %>% 
  restore_zeros(width = 2)
#> [1] "4.00" "6.90" "5.00" "4.20" "4.80" "7.00" "4.00"

vec2 %>% 
  restore_zeros(width = 2) %>% 
  decimal_places()
#> [1] 2 2 2 2 2 2 2
```

### Sequence generation

#### Introduction

[`base::seq()`](https://rdrr.io/r/base/seq.html) offers a flexible way
to generate sequences, but it is not cut out for working with decimal
numbers. The `by` argument only allows for manual specifications of the
step size, i.e., the difference between two consecutive output values.
In an error detection context, there is also the problem of trailing
zeros in numeric values.

Use scrutiny’s `seq_*()` functions to automatically determine step size
from the input numbers and, by default, to supply missing trailing zeros
via
[`restore_zeros()`](https://lhdjung.github.io/scrutiny/reference/restore_zeros.md).
Output will then naturally be string.

Why are there multiple such functions? The first two disentangle the two
different ways in which [`seq()`](https://rdrr.io/r/base/seq.html) can
be used. A third function adds a way of generating sequences not
directly covered by [`seq()`](https://rdrr.io/r/base/seq.html).

- [`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  takes two main arguments, `from` and `to`. It creates a sequence
  between the two, inferring step size from the greater number of
  decimal places among them. This corresponds to a
  [`seq()`](https://rdrr.io/r/base/seq.html) call in which `to` was
  specified.

- [`seq_distance()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
  takes a `from` argument, uses it to infer the step size, and creates a
  sequence of a length specified by the `length_out` argument (default
  is `10`). This corresponds to a
  [`seq()`](https://rdrr.io/r/base/seq.html) call in which `length.out`
  was specified.

- Finally,
  [`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
  creates a sequence centered around `from`.

Each of these functions has a `*_df()` variant that embeds the sequence
as a tibble column.

#### Examples

The `seq_*()` functions have some more features, such as offsets and
direction reversal, but I’ll focus on the basics here.

Call
[`seq_endpoint()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
to bridge two numbers at the correct decimal level:

``` r
seq_endpoint(from = 4.1, to = 6)
#>  [1] "4.1" "4.2" "4.3" "4.4" "4.5" "4.6" "4.7" "4.8" "4.9" "5.0" "5.1" "5.2"
#> [13] "5.3" "5.4" "5.5" "5.6" "5.7" "5.8" "5.9" "6.0"

seq_endpoint(from = 4.1, to = 4.15)
#> [1] "4.10" "4.11" "4.12" "4.13" "4.14" "4.15"
```

Call
[`seq_distance()`](https://lhdjung.github.io/scrutiny/reference/seq-decimal.md)
to get a sequence of desired length:

``` r
seq_distance(from = 4.1, length_out = 3)
#> [1] "4.1" "4.2" "4.3"

# Default for `length_out` is `10`:
seq_distance(from = 4.1)
#>  [1] "4.1" "4.2" "4.3" "4.4" "4.5" "4.6" "4.7" "4.8" "4.9" "5.0"
```

Finally, call
[`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
to construct a sequence around `from`:

``` r
seq_disperse(from = 4.1, dispersion = 1:3)
#> [1] "3.8" "3.9" "4.0" "4.1" "4.2" "4.3" "4.4"

# Default for `dispersion` if `1:5`:
seq_disperse(from = 4.1)
#>  [1] "3.6" "3.7" "3.8" "3.9" "4.0" "4.1" "4.2" "4.3" "4.4" "4.5" "4.6"
```

[`seq_disperse()`](https://lhdjung.github.io/scrutiny/reference/seq_disperse.md)
is a hybrid between the two [`seq()`](https://rdrr.io/r/base/seq.html)
wrappers explained above and the `disperse*()` functions introduced
next.

### Sequence testing

#### General points

Four predicate functions test whether a vector `x` represents particular
kinds of sequences. These testing functions can be used as helpers, but
they are also analytic tools in their own right.

[`is_seq_linear()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
returns `TRUE` if the difference between all neighboring values is the
same:

``` r
is_seq_linear(x = 8:15)
#> [1] TRUE
is_seq_linear(x = c(8:15, 16))
#> [1] TRUE
is_seq_linear(x = c(8:15, 17))
#> [1] FALSE
```

[`is_seq_ascending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
tests whether that difference is always positive…

``` r
is_seq_ascending(x = 8:15)
#> [1] TRUE
is_seq_ascending(x = 15:8)
#> [1] FALSE

# Default also tests for linearity:
is_seq_ascending(x = c(8:15, 17))
#> [1] FALSE
is_seq_ascending(x = c(8:15, 17), test_linear = FALSE)
#> [1] TRUE
```

…whereas
[`is_seq_descending()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
tests whether it is always negative:

``` r
is_seq_descending(x = 8:15)
#> [1] FALSE
is_seq_descending(x = 15:8)
#> [1] TRUE

# Default also tests for linearity:
is_seq_descending(x = c(15:8, 2))
#> [1] FALSE
is_seq_descending(x = c(15:8, 2), test_linear = FALSE)
#> [1] TRUE
```

[`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md)
tests whether the vector is grouped around its `from` argument:

``` r
is_seq_dispersed(x = 3:7, from = 2)
#> [1] FALSE

# Direction doesn't matter here:
is_seq_dispersed(x = 3:7, from = 5)
#> [1] TRUE
is_seq_dispersed(x = 7:3, from = 5)
#> [1] TRUE

# Dispersed from `50`, but not linear:
x_nonlinear <- c(49, 42, 47, 44, 50, 56, 53, 58, 51)

# Default also tests for linearity:
is_seq_dispersed(x = x_nonlinear, from = 50)
#> [1] FALSE
is_seq_dispersed(x = x_nonlinear, from = 50, test_linear = FALSE)
#> [1] TRUE
```

#### `NA` handling

All the `is_seq_*()` functions take special care with missing values. If
one or more elements of `x` are `NA`, this doesn’t necessarily mean that
it’s unknown whether or not `x` might possibly represent the kind of
sequence in question.

In these examples, it is genuinely unclear whether `x` is linear:

``` r
is_seq_linear(x = c(1, 2, NA, 4))
#> [1] NA
is_seq_linear(x = c(1, 2, NA, NA, NA, 6))
#> [1] NA
```

Linearity thus depends on the unknown, missing value behind `NA`:

``` r
is_seq_linear(x = c(1, 2, 3, 4))
#> [1] TRUE
is_seq_linear(x = c(1, 2, 7, 4))
#> [1] FALSE

is_seq_linear(x = c(1, 2, 3, 4, 5, 6))
#> [1] TRUE
is_seq_linear(x = c(1, 2, 17, 29, 32, 6))
#> [1] FALSE
```

Sometimes, however, `x` cannot possibly represent the tested kind of
sequence, independently of the hypothetical numbers substituted for `NA`
elements. In such cases, scrutiny’s `is_seq_*()` functions will always
return `FALSE`:

``` r
is_seq_linear(x = c(1, 2, NA, 10))
#> [1] FALSE
is_seq_linear(x = c(1, 2, NA, NA, NA, 10))
#> [1] FALSE
```

This is very much in the spirit of consistency testing. Even if certain
data are unknown, it still makes sense to check whether or not *any*
data could possibly fill in the gaps. The `is_seq_*()` functions
effectively ask: Are the numbers left and right of the `NA`s consistent
with each other, given their index positions?

It is worth emphasizing that this behavior is not exotic, or specific to
scrutiny. It simply asserts the fundamental ideas of `NA` propagation in
R. For example, `is_seq_ascending(x = c(1, 2, NA, 1))` is `FALSE` for
the same reason that `NA & FALSE` is `FALSE`: The outcome is the same
for all possible values of `NA` (Wickham 2019, ch. 3.2.3).

Leading and trailing `NA`s are mostly ignored when determining whether
`x` *might* be the kind of sequence in question:

``` r
is_seq_linear(x = c(NA, NA, 1, 2, 3, 4, NA))
#> [1] NA
is_seq_linear(x = c(NA, NA, 1, 2, NA, 4, NA))
#> [1] NA
```

The only exception,
[`is_seq_dispersed()`](https://lhdjung.github.io/scrutiny/reference/seq-predicates.md),
is particularly sensitive to `NA` values:

``` r
# `TRUE` because `x` is symmetrically dispersed
# from 5 and contains no `NA` values:
is_seq_dispersed(x = c(3:7), from = 5)
#> [1] TRUE

# `NA` because it might be dispersed from 5,
# depending on the values hidden behind the `NA`s:
is_seq_dispersed(x = c(NA, 3:7, NA), from = 5)
#> [1] NA
is_seq_dispersed(x = c(NA, NA, 3:7, NA, NA), from = 5)
#> [1] NA

# `FALSE` because it's not symmetrically dispersed
# around 5, no matter what the `NA`s stand in for:
is_seq_dispersed(x = c(NA, 3:7), from = 5)
#> [1] FALSE
is_seq_dispersed(x = c(3:7, NA), from = 5)
#> [1] FALSE
is_seq_dispersed(x = c(3:7, NA, NA), from = 5)
#> [1] FALSE
is_seq_dispersed(x = c(NA, NA, 3:7), from = 5)
#> [1] FALSE
```

### Disperse from (around) half with `disperse_total()`

Briefly,
[`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
checks if an input total is even or odd, cuts it in half, and creates
“dispersed” group sizes going out from there, with each pair of group
sizes adding up to the input total. This works naturally with even
totals. For odd totals, it starts with the two integers closest to half.

The function internally calls either of
[`disperse()`](https://lhdjung.github.io/scrutiny/reference/disperse.md)
and
[`disperse2()`](https://lhdjung.github.io/scrutiny/reference/disperse.md),
but I recommend simply using the higher-level
[`disperse_total()`](https://lhdjung.github.io/scrutiny/reference/disperse.md).
Here are two basic examples:

``` r
# With an even total...
disperse_total(n = 70)
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
#>  1    35        0
#>  2    35        0
#>  3    34       -1
#>  4    36        1
#>  5    33       -2
#>  6    37        2
#>  7    32       -3
#>  8    38        3
#>  9    31       -4
#> 10    39        4
#> 11    30       -5
#> 12    40        5

# ...and with an odd total:
disperse_total(n = 83)
#> # A tibble: 12 × 2
#>        n n_change
#>    <dbl>    <int>
#>  1    41        0
#>  2    42        0
#>  3    40       -1
#>  4    43        1
#>  5    39       -2
#>  6    44        2
#>  7    38       -3
#>  8    45        3
#>  9    37       -4
#> 10    46        4
#> 11    36       -5
#> 12    47        5
```

### Test for subsets, supersets, and equal sets

Starting with
[`is_subset_of()`](https://lhdjung.github.io/scrutiny/reference/subset-superset.md),
scrutiny features a distinctive family of predicate functions that test
whether one vector `x` is a subset of another vector `y`, whether `x` is
a superset of `y` (i.e. the reverse of a subset), or whether `x` and `y`
are equal sets.

As a teaser: These functions are divided into three subgroups based on
the way the second vector, `y`, is constituted. For example, you might
test if `x` is a subset of multiple other vectors taken together, or a
superset of a vector `y` that consists of multiple values entered along
with `x`.

Functions from this family are not currently used as helpers inside
other scrutiny functions, but that may well change. Use elsewhere is
also conceivable.

## References

Wickham, Hadley. 2019. *Advanced r*. Second edition. Boca Raton: CRC
Press/Taylor; Francis Group.
