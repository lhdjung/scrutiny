# Test for subsets, supersets, and equal sets

**\[deprecated\]**

All of these functions are deprecated and will be removed in a future
version. They are a poor fit for the problem they try to solve, and they
are far out of scope for the package.

### Original documentation

Predicate functions that take a vector and test whether it has some
particular relation to another vector. That second vector is entered in
either of three ways –

**Enter it directly (basic functions):**

`is_subset_of()` tests if a vector is a subset of another vector; i.e.,
if all its elements are contained in the second one. `is_superset_of()`
does the reverse: It tests if the first vector contains all elements of
the second one. `is_equal_set()` tests if both vectors have exactly the
same values.

**Enter its values:**

`is_subset_of_vals()`, `is_superset_of_vals()`, and
`is_equal_set_vals()` are variants that each take a single vector plus
any number of other arguments. These are treated like elements of the
second vector in the basic functions above.

**Enter multiple vectors that jointly contain its values:**

Finally, `is_subset_of_vecs()`, `is_superset_of_vecs()`, and
`is_equal_set_vecs()` take one vector plus any number of other vectors
and treat their elements (!) like elements of a second vector in the
basic functions above.

Each `is_subset*()` function has an `is_proper_subset*()` variant. These
variants also test whether the sets are unequal, so that `x` is a subset
of `y` but `y` is not a subset of `x`. The same applies to
`is_superset*()` functions and their `is_proper_superset*()` variants.

## Usage

``` r
is_subset_of(x, y)

is_superset_of(x, y)

is_equal_set(x, y)

is_proper_subset_of(x, y)

is_proper_superset_of(x, y)

is_subset_of_vals(x, ...)

is_superset_of_vals(x, ...)

is_equal_set_vals(x, ...)

is_proper_subset_of_vals(x, ...)

is_proper_superset_of_vals(x, ...)

is_subset_of_vecs(x, ...)

is_superset_of_vecs(x, ...)

is_equal_set_vecs(x, ...)

is_proper_subset_of_vecs(x, ...)

is_proper_superset_of_vecs(x, ...)
```

## Arguments

- x:

  A vector.

- y:

  A vector. Only in the basic functions, not those with `*_vals()` or
  `*_vecs()`.

- ...:

  In the `*_vals()` functions, any number of values `x` might contain;
  in the `*_vecs()` functions, any number of vectors the elements of
  which `x` might contain.

## Value

A single logical value. `TRUE` if the respective test was passed,
`FALSE` otherwise.

## Details

The `*_vals()` variants are meant for flexible, interactive
subset/superset testing. That is, in order to test whether certain
values collectively fulfill the role of the second vector, you can just
add them to the function call.

The `*_vecs()` variants likewise offer flexibility, but also bridge the
gap between vectors and values contained in them.

All functions simply check if values are present, regardless of how
often a value occurs. In other words, they look for types but don't
count tokens.

## Examples

``` r
# Define example vectors:
ab <- c("a", "b")
abc <- c("a", "b", "c")
abcde <- c("a", "b", "c", "d", "e")

# `is_subset_of()` tests if a vector is
# completely covered by another one:
abc %>% is_subset_of(ab)
#> Warning: `is_subset_of()` was deprecated in scrutiny 0.5.0.
#> ℹ It will be removed in a future version.
#> [1] FALSE
abc %>% is_subset_of(abc)
#> [1] TRUE
abc %>% is_subset_of(abcde)
#> [1] TRUE

# To the contrary, `is_superset_of()` tests if the
# first vector completely covers the second one:
abc %>% is_superset_of(ab)
#> Warning: `is_superset_of()` was deprecated in scrutiny 0.5.0.
#> ℹ It will be removed in a future version.
#> [1] TRUE
abc %>% is_superset_of(abc)
#> [1] TRUE
abc %>% is_superset_of(abcde)
#> [1] FALSE

# `is_equal_set()` tests both of the above --
# i.e., if both vectors have exactly the
# same values:
abc %>% is_equal_set(ab)
#> Warning: `is_equal_set()` was deprecated in scrutiny 0.5.0.
#> ℹ It will be removed in a future version.
#> [1] FALSE
abc %>% is_equal_set(abc)
#> [1] TRUE
abc %>% is_equal_set(abcde)
#> [1] FALSE

# Each of the three functions has a `*_vals()` variant
# that doesn't take a second vector like the first
# one, but any number of other arguments. These are
# jointly treated like the elements of the second
# vector in the basic functions:
abc %>% is_subset_of_vals("a", "b")
#> Warning: `is_subset_of_vals()` was deprecated in scrutiny 0.5.0.
#> ℹ It will be removed in a future version.
#> [1] FALSE
abc %>% is_subset_of_vals("a", "b", "c")
#> [1] TRUE
abc %>% is_subset_of_vals("a", "b", "c", "d", "e")
#> [1] TRUE

# (... and likewise for supersets and equal sets.)
```
