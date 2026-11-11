# Technical notes

This vignette is about how scrutiny arrives at its results: missing
values, rounding, exact arithmetic, and decimal places. None of it is
needed to use the package. It is for those who want to know why a result
can be trusted, or why a call is refused.

## Missing values

A test result is `NA` if the case is not decided. A missing value only
makes it `NA` if some value in its place would make the value set
consistent and another would make it inconsistent. If every value gives
the same result, that result is returned instead. This is how R itself
treats `NA & FALSE`, which is `FALSE`.

A mean of 5.2 passes GRIM with an `n` of 5 or 10, but not with 7, so a
missing `n` leaves the case open. A whole-number mean passes with any
`n`:

``` r

grim(
  x = c(5.2, 5),
  n = NA,
  digits_x = c(1, 0),
  items_averaged = 1,
  data_type = "integer"
)
#> [1]   NA TRUE
```

A missing `digits_x` or `digits_sd` is read the same way. A value has at
least the decimal places it shows, and more of them only narrow the
range of numbers rounded to it. So a value set that fails with the
decimal places shown fails with any. And one that passes with so many
decimal places that only the value shown itself can pass, passes with
any: 26 / 5 is exactly 5.2.

``` r

grim(
  x = c(5.19, 5.2, 5.2),
  n = c(28, 5, 28),
  digits_x = NA,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] FALSE  TRUE    NA
```

A few cases are still `NA` although every value would give the same
result:

- GRIMMER with `n` or `items_averaged` missing is only `TRUE` if data
  that are all the same value fit, and only `FALSE` if the SD is
  negative or, with `items_averaged` known, below the smallest that
  whole numbers with the mean can have. An SE is never below such a
  floor.
- DEBIT with `n` missing, and an SD larger than any population SD with
  that mean, needs a small `n` to pass as a sample SD or an SE. It tries
  each `n` that could work, and is `NA` if these go beyond 1,000.
- GRIMMER and DEBIT with `x` or `sd` missing are never `TRUE`.
- A missing `digits_*` value is never `TRUE` if settling it takes more
  decimal places than exact arithmetic allows (see below), as for a
  large `n`, nor under DEBIT’s `formula = "mean_n"`.
- GRIM with `x` missing is undecided for some `n` if `digits_x` is 4 or
  more, or 2 or more for a percentage.

A `TRUE` or `FALSE` reached with missing values is always the result
that every value in their place would give.

## Rounding

A reported value was rounded from some unknown number, so the tests work
with the range of numbers that round to it. Which range that is depends
on the rounding method, and these come from the roundwork package.

roundwork gives each bound of the range as a whole number over a common
denominator, and says whether it is included. For 5.19 with two decimal
places and the default `rounding = "up_or_down"`, which accepts a tie
rounded either way:

``` r

roundwork::bound_numerators(
  5.19,
  digits = 2,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
#> $lower
#> [1] 5185
#> 
#> $upper
#> [1] 5195
#> 
#> $denom
#> [1] 1000
#> 
#> $incl_lower
#> [1] TRUE
#> 
#> $incl_upper
#> [1] TRUE
```

That is 5.185 to 5.195, both included. GRIM multiplies these bounds by
the sample size and asks whether a whole number lies in between.
[`roundwork::sum_range()`](https://lhdjung.github.io/roundwork/reference/sum_range.html)
returns the least and the greatest such number, so the range is empty if
the first is greater:

``` r

roundwork::sum_range(
  5.19,
  n_items = 28,
  digits = 2,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
#> [1] 146 145
roundwork::sum_range(
  5.18,
  n_items = 28,
  digits = 2,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
)
#> [1] 145 145
```

GRIMMER and DEBIT build on the same bounds. `"even"` keeps both bounds
inclusive; see
[`roundwork::unround()`](https://lhdjung.github.io/roundwork/reference/unround.html)
on why. The `"ties_*"` methods each name a complete rule for ties, so
they rule out `symmetric = TRUE`.

roundwork checks `rounding`, `threshold`, and `symmetric`, but its
errors would name its own functions. scrutiny checks them first, so that
the error names the function that was called.

## Exact arithmetic

A value set that sits right on a rounding bound passes or fails by the
bound alone. If the comparison were made in floating-point numbers,
representation error would decide it instead. Take a mean of 1.03 from
40 integers. Their sum must be 41: 41 / 40 is 1.025, which rounds up to
1.03. But in floating-point numbers, the lower bound times 40 comes out
a little above 41, so the sum would seem to be 42 or more, and the mean
inconsistent:

``` r

(1.03 - 0.005) * 40 > 41
#> [1] TRUE
```

scrutiny never computes this product. roundwork gives the bounds as
whole numbers over a common denominator, here 1025 and 1035 over 1000
(see above), and GRIM asks whether some whole number `k` meets
`1025 * 40 <= 1000 * k <= 1035 * 40`. Every number in this comparison is
whole, so it is exact:

``` r

grim(
  x = 1.03,
  n = 40,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] TRUE
```

All three tests compare whole numbers in this way. A double holds them
exactly below 2^53, about 9e15.

GRIM’s products stay below that for any realistic value set. Beyond it,
GRIM refuses the value set rather than return a result it can’t vouch
for:

``` r

grim(
  x = 5.19,
  n = 1e14,
  digits_x = 2,
  items_averaged = 1,
  data_type = "integer"
)
#> Error in `grim()`:
#> ! `x` times the sample size is too large for an exact test.
#> ✖ The sums that `x` admits are compared as whole numbers, which is exact only
#>   below 2^53, about 9.007e15, and `(x * 10^(digits_x + 1) + 10) * n` is about
#>   5.2e17.
#> ℹ `items_averaged` multiplies into `n` here.
```

GRIMMER and DEBIT square their values, which takes them past 2^53
sooner. They compare such products exactly with Dekker’s algorithm,
which splits each into a double and the remainder, and past that, they
compute with whole numbers of any size. So they decide value sets of any
size exactly, apart from a few refusals of sizes that would take too
long and that no study reports, such as a sample SD of more than about
1.05 million (2^20) for two values:

``` r

grimmer(
  x = 1,
  sd = 1.1e6,
  n = 2,
  digits_x = 0,
  digits_sd = 0,
  items_averaged = 1,
  data_type = "integer"
)
#> Error in `grimmer()`:
#> ! The SD is too large for GRIMMER with `n` from 2 to 4.
#> ✖ With `n = 2`, the sums of squares to test reach 2^40, about 1.1e12, beyond
#>   which telling which of them 2 whole numbers can have is slow.
#> ℹ With one or five or more values, `grimmer()` has no such limit.
```

## Decimal places

A number in R drops its trailing zeros: `3.70` is stored as `3.7`. A
reported mean of 3.70 says more than one of 3.7, and the tests depend on
it, so the number of decimal places is an argument of its own, such as
`digits_x`.
[`decimal_places()`](https://lhdjung.github.io/scrutiny/reference/decimal_places.md)
counts them from strings, which keep the zeros:

``` r

decimal_places(c("3.70", 3.70))
#> [1] 2 1
```

Arithmetic leaves noise in the last bits of a double: `100 * 0.29` is
not exactly 29. Arguments that must be whole numbers, such as `n` and
`digits_x`, are accepted as whole if they are within about `1.5e-8` of
one, and then tested as that whole number:

``` r

100 * 0.29 == 29
#> [1] FALSE
grim(
  x = 5.2,
  n = 100 * 0.29,
  digits_x = 1,
  items_averaged = 1,
  data_type = "integer"
)
#> [1] TRUE
```

Numbers that scrutiny turns into strings, such as for counting decimal
places, are written out in full and with a point as the decimal mark,
whatever `options(scipen)` and `options(OutDec)` say.
