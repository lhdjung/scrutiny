# Exact arithmetic on whole numbers -------------------------------------------
#
# A double holds every whole number below `2^53` exactly, and past it skips
# some. The consistency tests compare whole numbers built from rounding bounds,
# where a value set right on a bound must compare as equal, so they can't let
# a product pass `2^53` unnoticed. Two levels of help:
#
# - `compare_products()` and its relatives compare or divide products of two
#   factors below `2^53` exactly, using doubles only. They are cheap enough for
#   the tests' common path.
# - `big()` and the `big_*()` functions work on whole numbers of any size,
#   stored as base-2^24 digits ("limbs"), least significant first. They are
#   slower, and the tests only use them when a factor or result passes `2^53`.

# Euclid's algorithm, exact for whole numbers below `2^53`. Reducing a rounding
# bound such as 1640625 / 10^6 to 105 / 64 keeps the products that the tests
# compare it in far below that limit:
gcd <- function(a, b) {
  while (b > 0) {
    remainder <- a %% b
    a <- b
    b <- remainder
  }
  a
}

lowest_terms <- function(num, den) {
  divisor <- gcd(abs(num), den)
  list(num = num / divisor, den = den / divisor)
}


# The sign of `a * b - c * d` for whole numbers below `2^53`, exact although
# the products may reach `2^106`. Each product is the double nearest to it plus
# the exact remainder, found by splitting the factors into halves of 26 bits
# (Dekker, 1971). Rounding to the nearest double never reverses an order, so
# the nearest doubles decide unless they are equal, and the remainders then do.
compare_products <- function(a, b, c, d) {
  ab <- two_product(a, b)
  cd <- two_product(c, d)
  if (ab[[1L]] != cd[[1L]]) {
    return(sign(ab[[1L]] - cd[[1L]]))
  }
  sign(ab[[2L]] - cd[[2L]])
}

two_product <- function(a, b) {
  product <- a * b
  a_high <- split_high(a)
  b_high <- split_high(b)
  a_low <- a - a_high
  b_low <- b - b_high
  remainder <- ((a_high * b_high - product) +
    a_high * b_low +
    a_low * b_high) +
    a_low * b_low
  c(product, remainder)
}

split_high <- function(a) {
  scaled <- 134217729 * a # 2^27 + 1
  scaled - (scaled - a)
}


# `a * b - c * d` for whole numbers below `2^53` whose products may pass it,
# as long as the difference itself is below `2^53`. The nearest doubles of the
# products are then so close that subtracting them is exact (Sterbenz), and so
# is subtracting the remainders, which are whole numbers below `2^53`.
product_difference <- function(a, b, c, d) {
  ab <- two_product(a, b)
  cd <- two_product(c, d)
  (ab[[1L]] - cd[[1L]]) + (ab[[2L]] - cd[[2L]])
}


# `floor(a * b / d)` for whole numbers below `2^53` whose quotient is below it,
# too, plus whether the division is exact. The quotient in floating point is
# off by a few units at most, which `compare_products()` corrects:
floor_product_ratio <- function(a, b, d) {
  q <- floor(a * b / d)
  # Not a whole number any more, let alone exact; the callers refuse it:
  if (q >= 2^53) {
    return(list(floor = q, exact = FALSE))
  }
  while (q > 0 && compare_products(q, d, a, b) > 0) {
    q <- q - 1
  }
  while (compare_products(q + 1, d, a, b) <= 0) {
    q <- q + 1
  }
  list(floor = q, exact = compare_products(q, d, a, b) == 0)
}


# Whole numbers of any size --------------------------------------------------

# Limbs are base 2^24, so that the product of two is below 2^48, and a column
# of a product can add up some 30 of them below 2^53. The numbers here have a
# dozen limbs at most: products of four factors below 2^53 or so.
BIG_BASE <- 2^24

# A whole double of any size, exactly: dividing by a power of two is exact, and
# so is taking off the multiple of the base that `floor()` leaves.
big <- function(x) {
  out <- numeric(0L)
  repeat {
    high <- floor(x / BIG_BASE)
    out <- c(out, x - high * BIG_BASE)
    x <- high
    if (x == 0) {
      return(out)
    }
  }
}

# Without leading zeros, but at least one limb:
big_trim <- function(a) {
  nonzero <- which(a != 0)
  if (length(nonzero) == 0L) 0 else a[seq_len(max(nonzero))]
}

# Each limb into the base, carrying the rest to the next one. A limb may be
# negative after a subtraction, which `floor()` turns into a borrow:
big_carry <- function(a) {
  a <- c(a, 0)
  for (i in seq_len(length(a) - 1L)) {
    carry <- floor(a[i] / BIG_BASE)
    a[i] <- a[i] - carry * BIG_BASE
    a[i + 1L] <- a[i + 1L] + carry
  }
  big_trim(a)
}

big_add <- function(a, b) {
  length_out <- max(length(a), length(b))
  big_carry(
    c(a, numeric(length_out - length(a))) +
      c(b, numeric(length_out - length(b)))
  )
}

# `a - b` for `a >= b`:
big_sub <- function(a, b) {
  big_carry(a - c(b, numeric(length(a) - length(b))))
}

big_mul <- function(a, b) {
  out <- numeric(length(a) + length(b))
  for (i in seq_along(a)) {
    index <- i - 1L + seq_along(b)
    out[index] <- out[index] + a[i] * b
  }
  big_carry(out)
}

# The sign of `a - b`:
big_cmp <- function(a, b) {
  a <- big_trim(a)
  b <- big_trim(b)
  if (length(a) != length(b)) {
    return(sign(length(a) - length(b)))
  }
  differ <- which(a != b)
  if (length(differ) == 0L) 0 else sign(a[max(differ)] - b[max(differ)])
}

# The nearest double, or close to it; exact below `2^53`:
big_double <- function(a) {
  sum(a * BIG_BASE^(seq_along(a) - 1L))
}

big_is_odd <- function(a) {
  a[1L] %% 2 == 1
}

# `floor(a / b)` and the remainder, for `b > 0`. The quotient in floating point
# is right to about 16 significant digits; each round corrects it by the
# remainder's quotient, so that the error shrinks by as much every time, until
# the remainder is below `b`.
big_divmod <- function(a, b) {
  q <- big(floor(big_double(a) / big_double(b)))
  repeat {
    product <- big_mul(q, b)
    if (big_cmp(product, a) > 0) {
      excess <- big_sub(product, a)
      step <- max(ceiling(big_double(excess) / big_double(b)), 1)
      q <- big_sub(q, big(min(step, big_double(q))))
    } else {
      remainder <- big_sub(a, product)
      if (big_cmp(remainder, b) < 0) {
        return(list(quotient = q, remainder = remainder))
      }
      step <- max(floor(big_double(remainder) / big_double(b)), 1)
      q <- big_add(q, big(step))
    }
  }
}

# The number in decimal digits, for explanations:
big_format <- function(a) {
  a <- big_trim(a)
  groups <- character(0L)
  repeat {
    # Long division by 10^7: each step's dividend is below 10^7 * 2^24, so
    # below 2^53.
    remainder <- 0
    for (i in rev(seq_along(a))) {
      current <- remainder * BIG_BASE + a[i]
      a[i] <- floor(current / 1e7)
      remainder <- current - a[i] * 1e7
    }
    a <- big_trim(a)
    if (length(a) == 1L && a == 0) {
      head <- formatC(remainder, format = "f", digits = 0L)
      return(paste0(c(head, rev(groups)), collapse = ""))
    }
    groups <- c(
      groups,
      formatC(remainder, width = 7L, flag = "0", format = "f", digits = 0L)
    )
  }
}


# Whole numbers that are doubles below `2^53` and `big()` numbers from there on,
# classed `scrutiny_big`, so that any big one is greater than any double:
big_or_double <- function(a) {
  if (big_cmp(a, big(2^53)) < 0) {
    big_double(a)
  } else {
    structure(big_trim(a), class = "scrutiny_big")
  }
}

is_big <- function(x) {
  inherits(x, "scrutiny_big")
}

whole_add <- function(x, k) {
  if (!is_big(x)) {
    return(x + k)
  }
  x <- unclass(x)
  big_or_double(if (k >= 0) big_add(x, big(k)) else big_sub(x, big(-k)))
}

# The sign of `a - b`:
whole_cmp <- function(a, b) {
  if (is_big(a) && is_big(b)) {
    big_cmp(unclass(a), unclass(b))
  } else if (is_big(a)) {
    1
  } else if (is_big(b)) {
    -1
  } else {
    sign(a - b)
  }
}

whole_is_odd <- function(x) {
  if (is_big(x)) big_is_odd(unclass(x)) else x %% 2 == 1
}

whole_format <- function(x) {
  if (is_big(x)) {
    big_format(unclass(x))
  } else {
    formatC(x, format = "f", digits = 0L)
  }
}

# `a * b` for whole numbers `a, b >= 0`, each a double or a `big()` number. A
# product of doubles below `2^52` is exact, as the true one is then below
# `2^53`:
whole_mul <- function(a, b) {
  if (!is_big(a) && !is_big(b)) {
    product <- a * b
    if (product < 2^52) {
      return(product)
    }
  }
  big_or_double(big_mul(as_big(a), as_big(b)))
}

as_big <- function(x) {
  if (is_big(x)) unclass(x) else big(x)
}

# The sign of `prod(left) - prod(right)` for whole numbers `>= 0`, each a
# double or a `big()` number, given as lists. Two factors on each side go to
# `compare_products()` if that is exact: the largest factor alone, the others
# multiplied out below `2^52`. Anything else is multiplied out in `big()`
# numbers.
compare_factors <- function(left, right) {
  split_two <- function(factors) {
    if (any(vapply(factors, is_big, logical(1L)))) {
      return(NULL)
    }
    factors <- sort(unlist(factors), decreasing = TRUE)
    rest <- prod(factors[-1L])
    if (factors[[1L]] >= 2^53 || rest >= 2^52) NULL else c(factors[[1L]], rest)
  }
  left_two <- split_two(left)
  right_two <- split_two(right)
  if (!is.null(left_two) && !is.null(right_two)) {
    return(compare_products(
      left_two[[1L]],
      left_two[[2L]],
      right_two[[1L]],
      right_two[[2L]]
    ))
  }
  big_cmp(
    Reduce(big_mul, lapply(left, as_big)),
    Reduce(big_mul, lapply(right, as_big))
  )
}

whole_double <- function(x) {
  if (is_big(x)) big_double(unclass(x)) else x
}
