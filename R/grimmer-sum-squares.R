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

  # Three or four numbers can have most sums of squares, so the first window
  # of `sum_squares_attainable_all()`, which has their rules, usually finds one:
  found <- sum_squares_attainable_all(s, n, first, upper, count_max = 0)
  if (length(found) == 0L) NA_real_ else found
}


# Every sum of squares from `first` to `upper` that `n < 5` whole numbers with
# the sum `s` can have, but no more than `count_max + 1`: enough to tell that
# there are too many. `first` must be at least `sum_squares_min(s, n)` and have
# the parity of `s`. The rules for three and four numbers are here only;
# `sum_squares_attainable()` uses them for the first one.
sum_squares_attainable_all <- function(
  s,
  n,
  first,
  upper,
  count_max,
  chunk = 1e5
) {
  # Squares are sparse, and `sum_squares_attainable()` jumps to each next one:
  if (n <= 2) {
    out <- numeric(0L)
    q <- first - 2
    k <- 0L
    while (k <= count_max) {
      q <- sum_squares_attainable(s, n, q + 2, upper)
      if (is.na(q)) {
        break
      }
      k <- k + 1L
      # Assigning past the end grows the vector with room to spare; `c()` would
      # copy it every time:
      out[k] <- q
    }
    return(out)
  }

  minimum <- sum_squares_min(s, n)
  r <- s %% n
  if (r > 0) {
    r <- min(r, n - r)
  }

  # In windows of sums of squares that start at 8 and double up to `chunk`:
  # the first attainable one is usually near, and a huge range is never
  # allocated at once.
  found <- list()
  count <- 0
  from <- first
  width <- min(8, chunk)

  while (from <= upper && count <= count_max) {
    to <- min(from + 2 * (width - 1), upper)

    # Three numbers: the half excess, or three times it plus one, is of the form
    # `x^2 + x * y + y^2`. Four: the excess is a sum of three squares, or not
    # 6 modulo 8 for a remainder of 2, or anything for one of 1.
    q <- if (n == 3) {
      half <- (c(from, to) - minimum) / 2
      if (r == 0) {
        minimum + 2 * loeschian_between(half[1L], half[2L])
      } else {
        m <- loeschian_between(3 * half[1L] + 1, 3 * half[2L] + 1)
        minimum + 2 * (m[m %% 3 == 1] - 1) / 3
      }
    } else {
      q <- seq(from, to, by = 2)
      excess <- q - minimum
      q[if (r == 0) is_sum_three_squares(excess) else r == 1 | excess %% 8 != 6]
    }

    found[[length(found) + 1L]] <- q
    count <- count + length(q)
    from <- to + 2
    width <- min(2 * width, chunk)
  }

  # Not `NULL` if there were no windows at all:
  out <- as.numeric(unlist(found))
  out[seq_len(min(length(out), count_max + 1))]
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


# The Loeschian numbers from `lo` to `hi`, generated as `x^2 + x * y + y^2`
# with `x >= y >= 0`: testing each number costs a scan of about `sqrt(m)`. For
# each `y`, the `x` with a value in range form a run.
loeschian_between <- function(lo, hi) {
  y <- 0:(floor(sqrt(hi / 3)) + 1)
  # From the root of `x^2 + x * y + y^2 = m`, widened by one against
  # floating-point error; the exact filter below trims the excess. Clamped by
  # subassignment, which is much cheaper than `pmax()` on short vectors, as most
  # of these are:
  root_lo <- 4 * lo - 3 * y^2
  root_lo[root_lo < 0] <- 0
  root_hi <- 4 * hi - 3 * y^2
  root_hi[root_hi < 0] <- 0
  x_from <- ceiling((sqrt(root_lo) - y) / 2) - 1
  x_from[x_from < y] <- y[x_from < y]
  x_to <- floor((sqrt(root_hi) - y) / 2) + 1
  runs <- x_to - x_from + 1
  runs[runs < 0] <- 0
  # Doubles, not the integers of `sequence()` and `:`, whose products overflow
  # past about 2e9:
  x <- as.numeric(sequence(runs, from = x_from))
  y <- rep(as.numeric(y), runs)
  m <- x^2 + x * y + y^2
  m <- m[m >= lo & m <= hi]
  # Sorted and unique: `sort()` costs some 20 microseconds even for a handful of
  # numbers, and the window is short enough to tabulate:
  lo - 1 + which(tabulate(m - lo + 1, nbins = hi - lo + 1) > 0)
}

# Legendre's three-square theorem: `m >= 0` is a sum of three squares unless it
# is `4^a * (8 * b + 7)`. Vectorized.
is_sum_three_squares <- function(m) {
  repeat {
    divisible <- m > 0 & m %% 4 == 0
    if (!any(divisible)) {
      break
    }
    m[divisible] <- m[divisible] / 4
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
