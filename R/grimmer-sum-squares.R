# Exact sum-of-squares arithmetic for GRIMMER -------------------------------

# GRIMMER's candidate sums come from `roundwork::sum_range()`, which works in
# exact integer arithmetic: every rounding bound is a whole number of units of
# `1 / 10^(digits + 1)` (`roundwork::bound_numerators()`), so comparing a mean
# to a bound becomes a comparison of integers (#86). The functions below extend
# this to the sums of squares that GRIMMER's test 1 needs.
#
# They work on numbers shifted to a sum below `n`. Subtracting a whole number
# `q` from each of `n` numbers subtracts `n * q` from their sum and keeps their
# deviations, so with `s = n * q + r`, the sum `s` passes the tests exactly if
# the remainder `r` does, with every sum of squares lowered by `q * (s + r)`.
# Its range then depends on the SD alone: from `r^2 / n` plus the SD's term at
# the lower bound to the same at the upper bound. The size of the mean no longer
# matters, and the numbers stay below `2^53` unless the SD itself, times `n`,
# gets very large.
#
# Even so, the SD's term multiplies `n_dev` by a squared fraction, and these
# products can pass `2^53`. `sd_square_term()` keeps them in doubles where they
# are exact (`compare_products()` and its relatives), and otherwise switches to
# whole numbers of any size (`big()`); see `R/exact-arithmetic.R`.

# The sum of squared deviations at an SD bound `num / denom` is `n_dev *
# (num / denom)^2 * items^2`, where `n_dev` is `n - 1` for a sample SD, `n` for
# a population SD, and `n * (n - 1)` for an SE (`sd_denominator()`). It does not
# depend on the candidate sum, so it is computed once per bound, outside
# GRIMMER's loop. In doubles, it is split into a whole part `int` and a fraction
# `num / den`; with too large a factor, it is kept as whole numbers of any size
# instead (`big = TRUE`). `approx` is its value as a double, for estimates.

sd_square_term <- function(num, n, items, denom, sd_meaning) {
  # In lowest terms first, which keeps the numbers small: an SD bound of
  # 1.640625 is 105/64, not 1640625/10^6.
  bound <- lowest_terms(num, denom)
  n_dev <- sd_denominator(n, sd_meaning)

  # Below this, `num * items` and `den` square to less than `2^53`:
  root_max <- sqrt(2^53)
  in_doubles <- bound$num * items < root_max &&
    bound$den < root_max &&
    n_dev < 2^53

  if (in_doubles) {
    numerator <- (bound$num * items)^2
    denom_sq <- bound$den^2
    # The common case: the product itself is below `2^53`, and so exact.
    # Otherwise, `floor_product_ratio()` and `product_difference()` are exact
    # for it:
    product <- n_dev * numerator
    if (product < 2^53) {
      whole <- floor(product / denom_sq)
    } else {
      whole <- floor_product_ratio(n_dev, numerator, denom_sq)$floor
    }
    # The shifted sums of squares add less than `n` to this:
    if (whole + n + 2 < 2^53) {
      remainder <- if (product < 2^53) {
        product - whole * denom_sq
      } else {
        product_difference(n_dev, numerator, whole, denom_sq)
      }
      return(list(
        int = whole,
        num = remainder,
        den = denom_sq,
        approx = whole + remainder / denom_sq
      ))
    }
  }

  # `n_dev` as a product, since `n * (n - 1)` can pass `2^53` on its own:
  n_dev_big <- switch(
    sd_meaning,
    sd_pop = big(n),
    se = big_mul(big(n), big(n - 1)),
    big(n - 1)
  )
  num_items <- big_mul(big(bound$num), big(items))
  numerator <- big_mul(num_items, num_items)
  denom_sq <- big_mul(big(bound$den), big(bound$den))
  term_scaled <- big_mul(n_dev_big, numerator)
  # Split as in doubles, with the remainder over `denom_sq`. GRIMMER's loop
  # compares the remainder, times `n`, with `denom_sq` times a whole number;
  # see `sum_squares_floor()`:
  division <- big_divmod(term_scaled, denom_sq)
  list(
    big = TRUE,
    int = big_or_double(division$quotient),
    num_n = big_mul(division$remainder, big(n)),
    num_zero = big_cmp(division$remainder, 0) == 0,
    denom_sq = denom_sq,
    approx = big_double(term_scaled) / big_double(denom_sq)
  )
}


# The range of sums of squares, shifted as described above, that `n` whole
# numbers with the sum `r` (`0 <= r < n`) can have under the SD's bounds,
# before asking which of them whole numbers attain. A list of `lower` and
# `upper`, each a double if below `2^53`, else a `big()` number; if `lower` is
# greater than `upper`, none fits.
sum_squares_range_shifted <- function(r, n, sd_bounds) {
  low <- sum_squares_floor(r, n, sd_bounds$term_lower)
  lower <- low$floor
  if (!low$exact || !sd_bounds$incl_lower) {
    lower <- whole_add(lower, 1)
  }

  high <- sum_squares_floor(r, n, sd_bounds$term_upper)
  upper <- high$floor
  if (high$exact && !sd_bounds$incl_upper) {
    upper <- whole_add(upper, -1)
  }

  list(lower = lower, upper = upper)
}


# The floor of `r^2 / n` plus the SD's term at one bound, and whether that sum
# is a whole number, which an exclusive bound needs to know.
sum_squares_floor <- function(r, n, term) {
  # `r^2 / n` as whole part and remainder. `r^2` only passes `2^53` with an `n`
  # of some 1e8:
  if (r < sqrt(2^53)) {
    whole_r <- floor(r^2 / n)
    remainder_r <- r^2 - whole_r * n
  } else {
    whole_r <- floor_product_ratio(r, r, n)$floor
    remainder_r <- product_difference(r, r, whole_r, n)
  }

  # With a term in `big()` numbers, `remainder_r / n` and `num / denom_sq`
  # carry 1 if `num * n >= denom_sq * (n - remainder_r)`:
  if (isTRUE(term$big)) {
    comparison <- big_cmp(
      term$num_n,
      big_mul(term$denom_sq, big(n - remainder_r))
    )
    exact <- comparison == 0 || (remainder_r == 0 && term$num_zero)
    return(list(
      floor = whole_add(term$int, whole_r + (comparison >= 0)),
      exact = exact
    ))
  }

  # The two fractions, `term$num / term$den` and `remainder_r / n`, are each
  # below 1, so they carry 0 or 1. Over the common denominator if that is
  # exact, which is cheaper; else by comparing `term$num / term$den` with
  # `1 - remainder_r / n` as products:
  if (term$den * n < 2^52) {
    numerator <- term$num * n + remainder_r * term$den
    denominator <- term$den * n
    carry <- floor(numerator / denominator)
    exact <- numerator == carry * denominator
  } else if (term$num == 0 && remainder_r == 0) {
    carry <- 0
    exact <- TRUE
  } else {
    comparison <- compare_products(term$num, n, term$den, n - remainder_r)
    carry <- as.numeric(comparison >= 0)
    exact <- comparison == 0
  }

  list(floor = term$int + whole_r + carry, exact = exact)
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
