# Exact sum-of-squares arithmetic for GRIMMER -------------------------------

# GRIMMER's candidate sums come from `roundwork::sum_range()`, which works in
# exact integer arithmetic: every rounding bound is a whole number of units of
# `1 / 10^(digits + 1)` (`roundwork::bound_numerators()`), so comparing a mean
# to a bound becomes a comparison of integers (#86). The functions below extend
# this to the sums of squares that GRIMMER's test 1 needs.
#
# This is exact while every integer involved stays below `2^53`. The products
# are on the order of `n * 10^(2 * digits)`, far below it for any realistic
# input; beyond it the arithmetic degrades to plain floating point.
#
# Dividing two such integers is exact as well, so `floor(a / b)` and
# `ceiling(a / b)` need no correction. IEEE 754 division returns the double
# nearest the true quotient, and a quotient that is not an integer lies at least
# `1 / b` from every integer -- more than half the spacing of doubles there, as
# long as `|a| < 2^53`. Don't add a check that multiplies the result back out:
# below that limit it never corrects anything, and above it, where the product
# rounds, it turns correct results into wrong ones more often than the reverse.

# The floor of a sum of two fractions, `a1 / b1 + a2 / b2` (`b1`, `b2` positive
# integers), plus a flag for whether the sum is an integer, which callers need
# to honor an exclusive bound. Splitting each fraction into integer part and
# remainder first keeps every product below `2 * b1 * b2`; putting both over
# `b1 * b2` directly would leave the exact range far sooner.

floor_frac_sum <- function(a1, b1, a2, b2) {
  q1 <- floor(a1 / b1)
  q2 <- floor(a2 / b2)
  # Both remainders are non-negative and smaller than their own denominator, so
  # the combined numerator below stays under `2 * b1 * b2`:
  num <- (a1 - q1 * b1) * b2 + (a2 - q2 * b2) * b1
  den <- b1 * b2
  # The fractional parts sum to less than 2, so this carry is 0 or 1:
  carry <- floor(num / den)
  list(floor = q1 + q2 + carry, exact = num == carry * den)
}


# The sum of squares of the item-level values is
#
#   ((n - 1) * sd^2 + n * (s / (n * items))^2) * items^2 ==
#     (n - 1) * sd^2 * items^2 + s^2 / n # nolint
#
# for a sample SD, and the same with `n` in place of `n - 1` for a population
# SD; `n_dev` is that denominator. With `sd` given as `num / denom`, both terms
# are exact rationals. The first does not depend on the candidate sum `s`, so it
# is pre-computed once per SD bound, outside GRIMMER's loop, split into an
# integer part and a proper fraction to keep the products small. Returns that
# integer part plus the numerator and denominator of the fraction.

sd_square_term <- function(num, n_dev, items, denom) {
  # `sd^2 * items^2` as a fraction over `denom^2`:
  numerator <- num^2 * items^2
  denom_sq <- denom^2
  whole <- floor(numerator / denom_sq)
  list(
    int = n_dev * whole,
    num = n_dev * (numerator - whole * denom_sq),
    den = denom_sq
  )
}


# The smallest sum of squares that `n` whole numbers with the sum `s` can have.
# With `s == n * q + r`, these are the numbers closest to each other: `r` of
# them are `q + 1` and the others are `q`. Any other numbers with the same sum
# are more spread out, so the sum of their squares is greater. Unlike `s^2 / n`,
# this bound is reached by whole numbers.

sum_squares_min <- function(s, n) {
  q <- s %/% n
  r <- s - n * q
  n * q^2 + r * (2 * q + 1)
}


# The smallest sum of squares from `lower` to `upper` that `n < 5` whole numbers
# with the sum `s` can have, or `NA` if there is none; `lower` must be at least
# `sum_squares_min(s, n)`. For `n >= 5`, every sum of squares from there on with
# the parity of `s` is possible, but for fewer numbers, some are not. Which ones
# depends on `r`, the remainder of `s` divided by `n`, and on the excess over
# the minimum.

sum_squares_attainable <- function(s, n, lower, upper) {
  minimum <- sum_squares_min(s, n)

  # Replacing each number `k` by `1 - k` turns `r` into `n - r` and keeps the
  # excess, so `r` and `n - r` share a rule:
  r <- s %% n
  if (r > 0) {
    r <- min(r, n - r)
  }

  # Start at the parity of `s`, the only one possible:
  first <- lower + (lower - s) %% 2
  if (first > upper) {
    return(NA_real_)
  }

  # One number has only the minimum. Two numbers, `q + e` and `q - e` (or
  # `q + 1 + e` and `q - e`), have the excess `2 * e^2` (or `2 * e * (e + 1)`,
  # so that `2 * excess + 1` is an odd square). These are too sparse to scan
  # for: with many items averaged, the range can run into the hundreds of
  # millions, and the next square is computed instead.
  if (n == 1) {
    return(if (first == minimum) minimum else NA_real_)
  }
  if (n == 2) {
    excess_min <- first - minimum
    q <- if (r == 0) {
      minimum + 2 * next_square_root(excess_min / 2)^2
    } else {
      k <- next_square_root(2 * excess_min + 1)
      if (k %% 2 == 0) {
        k <- k + 1
      }
      minimum + (k^2 - 1) / 2
    }
    return(if (q <= upper) q else NA_real_)
  }

  # Three or four numbers can have most sums of squares, so a scan from `first`
  # ends soon. Step by step, not `seq(first, upper, by = 2)`, which would
  # allocate the whole range: huge with many items averaged.
  q <- first
  while (q <= upper) {
    excess <- q - minimum
    half <- excess / 2
    attainable <- if (n == 3) {
      if (r == 0) is_loeschian(half) else is_loeschian(3 * half + 1)
    } else {
      if (r == 0) is_sum_three_squares(excess) else r == 1 || excess %% 8 != 6
    }
    if (attainable) {
      return(q)
    }
    q <- q + 2
  }

  NA_real_
}


# The smallest whole `k` with `k^2 >= m`, for `m >= 0`, corrected for
# floating-point error in `sqrt()`:
next_square_root <- function(m) {
  k <- ceiling(sqrt(m))
  if (k > 0 && (k - 1)^2 >= m) {
    k - 1
  } else if (k^2 < m) {
    k + 1
  } else {
    k
  }
}


# Is `m == x^2 + x * y + y^2` for some whole `x` and `y`? Equivalently, is
# `4 * m - 3 * y^2` a square (that of `2 * x + y`) for some `y`?
is_loeschian <- function(m) {
  if (m < 0) {
    return(FALSE)
  }
  y <- 0:floor(sqrt(4 * m / 3))
  any(round(sqrt(4 * m - 3 * y^2))^2 == 4 * m - 3 * y^2)
}

# Legendre's three-square theorem: `m >= 0` is a sum of three squares unless it
# is `4^a * (8 * b + 7)`.
is_sum_three_squares <- function(m) {
  while (m > 0 && m %% 4 == 0) {
    m <- m / 4
  }
  m %% 8 != 7
}


# Range of integer sums of squares consistent with candidate sum `s`, given the
# pre-computed SD terms from `sd_square_term()`. Returns a length-2 numeric
# vector; if the first element is greater than the second, no sum of squares
# fits, and the candidate sum fails GRIMMER's first test.

sum_squares_range <- function(
  s,
  n,
  term_lower,
  term_upper,
  incl_lower,
  incl_upper
) {
  s_squared <- s^2

  # An integer lower bound is its own ceiling; anything else, or an excluded
  # bound, takes the next integer up:
  low <- floor_frac_sum(term_lower$num, term_lower$den, s_squared, n)
  lower <- term_lower$int + low$floor
  if (!low$exact || !incl_lower) {
    lower <- lower + 1
  }

  high <- floor_frac_sum(term_upper$num, term_upper$den, s_squared, n)
  upper <- term_upper$int + high$floor
  if (high$exact && !incl_upper) {
    upper <- upper - 1
  }

  c(lower, upper)
}
