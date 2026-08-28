x <- runif(100000, min = 1900, max = 2100)
x_up <- x |> round_up(2) |> trunc_reverse()
x_down <- x |> round_down(2) |> trunc_reverse()

# Avoid whole numbers that will be rounded up, artificially leading to a
# difference of 1:
x_up <- x_up[x_up != 0]
x_down <- x_down[x_down != 0]


# Threshold 5 (via the wrappers) ------------------------------------------

# These are round trips: a number that already has two decimal places is a fixed
# point of rounding to two decimal places. They are smoke tests, and they cannot
# fail on tie behavior -- which is the whole reason these functions exist. The
# oracle tests further below cover that.

test_that("`round_up()` works correctly", {
  expect_equal(
    x_up,
    x_up |> trunc_reverse() |> round_up(2)
  )
})

test_that("`round_down()` works correctly", {
  expect_equal(
    x_down,
    x_down |> trunc_reverse() |> round_down(2)
  )
})


# The midpoint grid -------------------------------------------------------

# Ties are the only values at which these functions differ from each other and
# from every alternative, so they are the one input class that needs an oracle
# rather than a round trip. The expectations below are computed by hand from the
# definition of each procedure, not from any scrutiny function.

# The ten midpoints between consecutive first decimals: 0.05, 0.15, ..., 0.95.
# They are built from integers so that the test data does not itself depend on
# how a decimal literal is represented:
mid <- (seq(0, 9) * 10 + 5) / 100

test_that("`round_up()` sends positive ties to the higher neighbor", {
  # 0.05 -> 0.1, 0.15 -> 0.2, ..., 0.95 -> 1.0
  mid |> round_up(1) |> expect_equal((seq(0, 9) + 1) / 10)
})

test_that("`round_down()` sends positive ties to the lower neighbor", {
  # 0.05 -> 0.0, 0.15 -> 0.1, ..., 0.95 -> 0.9
  mid |> round_down(1) |> expect_equal(seq(0, 9) / 10)
})

test_that("`round_up()` sends ties toward `+Inf` by default", {
  # This is the default that `vignette("rounding-options")` now warns about: on
  # the number line, a negative tie goes up just like a positive one, so -0.05
  # -> 0.0 and -0.95 -> -0.9. Excel, SAS, and Matlab do *not* do this; see the
  # test after the next one.
  -mid |> round_up(1) |> expect_equal(-seq(0, 9) / 10)
})

test_that("`round_down()` sends ties toward `-Inf` by default", {
  -mid |> round_down(1) |> expect_equal(-(seq(0, 9) + 1) / 10)
})

test_that("`symmetric = TRUE` mirrors ties around zero", {
  # This is IEEE 754's *roundTiesToAway*, i.e. what Excel's `ROUND()`, SAS's
  # `ROUND()`, Matlab's `round()`, and `janitor::round_half_up()` do:
  -mid |> round_up(1, symmetric = TRUE) |> expect_equal(-(seq(0, 9) + 1) / 10)
  # ...and its mirror image, ties toward zero:
  -mid |> round_down(1, symmetric = TRUE) |> expect_equal(-seq(0, 9) / 10)
  # For non-negative numbers, `symmetric` changes nothing:
  mid |> round_up(1, symmetric = TRUE) |> expect_equal(round_up(mid, 1))
  mid |> round_down(1, symmetric = TRUE) |> expect_equal(round_down(mid, 1))
})

test_that("only ties are affected by the choice of procedure", {
  # Every value that is not a midpoint has a single nearest neighbor, so all
  # four variants have to agree with `base::round()` on it:
  non_ties <- c(
    seq(-999, 999) / 100,
    c(-9.273, -7.584, 7.584, 9.273, 0.101, -0.101)
  )
  non_ties <- non_ties[round(non_ties * 100) %% 5 != 0]
  for (symmetric in c(FALSE, TRUE)) {
    non_ties |> round_up(1, symmetric)   |> expect_equal(round(non_ties, 1))
    non_ties |> round_down(1, symmetric) |> expect_equal(round(non_ties, 1))
  }
})


# Custom thresholds -------------------------------------------------------

# `round_up_from()` rounds up whenever the part cut off by rounding is at least
# `threshold` tenths of a step, and `round_down_from()` rounds down whenever it
# is at most that many. `threshold` therefore means the same thing in both, and
# the two differ only in where they send a value sitting exactly on it.
#
# Up to scrutiny 1.0.0, `round_down_from()` was instead the point reflection of
# `round_up_from()`, switching direction at `10 - threshold`. The two agreed at
# the 5 that `round_up()` and `round_down()` round from and nowhere else, which
# left `rounding = "up_from_or_down_from"` spanning up to 1.8 steps -- see the
# width test at the bottom of this file.
#
# With `digits = 2` and a value carrying three decimals, the cut-off part is
# exactly the third decimal, so the expectation can be written down digit by
# digit.
#
# The blocks that used to stand here computed `x_up_from_3` and friends and then
# never used them: what they asserted was that re-rounding the *threshold-5*
# output reproduces itself, which is true for any threshold whatsoever. Custom
# thresholds therefore had no direct test at all.

cut_digit <- seq(0, 9)

test_that("`round_up_from()` rounds up from `threshold`, not from 5", {
  for (threshold in seq(1, 9)) {
    for (base in c(0.42, 7.13, 100)) {
      expect_equal(
        round_up_from(base + cut_digit / 1000, 2, threshold = threshold),
        base + ifelse(cut_digit >= threshold, 0.01, 0),
        label = paste("`round_up_from()`, threshold", threshold, "at", base)
      )
    }
  }
})

test_that("`round_down_from()` rounds down from `threshold`, not from 5", {
  for (threshold in seq(1, 9)) {
    for (base in c(0.42, 7.13, 100)) {
      expect_equal(
        round_down_from(base + cut_digit / 1000, 2, threshold = threshold),
        base + ifelse(cut_digit > threshold, 0.01, 0),
        label = paste("`round_down_from()`, threshold", threshold, "at", base)
      )
    }
  }
})

test_that("`round_down_from()` is `round_up_from()` with the tie reversed", {
  # The precise sense in which the two are a pair: they switch direction at the
  # same point, `threshold`, and differ only in which way a value sitting
  # exactly on that point goes. Exactly on the three-decimal grid, so that the
  # cut-off part really is the third decimal and `on_tie` below identifies it
  # without any rounding of its own:
  x <- seq(0, 9999) / 1000
  for (threshold in seq(1, 9)) {
    down <- round_down_from(x, 2, threshold = threshold)
    up <- round_up_from(x, 2, threshold = threshold)
    on_tie <- round(x * 1000) %% 10 == threshold
    expect_equal(down[!on_tie], up[!on_tie])
    # On the tie itself, `"down_from"` goes down and `"up_from"` goes up:
    expect_equal(down[on_tie] + 0.01, up[on_tie])
  }
})

test_that("a `threshold` of 5 makes the `*_from()` functions the plain ones", {
  x <- c(mid, -mid, seq(-500, 500) / 100)
  x |> round_up_from(1, threshold = 5) |> expect_equal(round_up(x, 1))
  x |> round_down_from(1, threshold = 5) |> expect_equal(round_down(x, 1))
  x |>
    round_up_from(1, threshold = 5, symmetric = TRUE) |>
    expect_equal(round_up(x, 1, symmetric = TRUE))
  x |>
    round_down_from(1, threshold = 5, symmetric = TRUE) |>
    expect_equal(round_down(x, 1, symmetric = TRUE))
})

test_that("`threshold` is rejected outside of the interval it has to lie in", {
  # At 0 or 10, one of the two directions can never be taken, which would
  # silently turn the method into a ceiling or a floor:
  # Not aligning pipes here because the lengths are too different
  0.5 |> reround(1, "up_from", threshold = 0) |> expect_error()
  0.5 |> reround(1, "up_from", threshold = 10) |> expect_error()
  0.5 |> reround(1, "down_from", threshold = -2) |> expect_error()
  0.5 |> reround(1, "up_from_or_down_from", threshold = NA) |> expect_error()
  "0.53" |> unround(rounding = "up_from", threshold = 12) |> expect_error()
  # ...whereas a fractional threshold inside it is fine. `unround()` scales the
  # bounds up until they are whole numbers again, so this also exercises the
  # rescaling loop in `bound_numerators()`:
  0.1445 |> reround(3, "up_from", threshold = 4.5) |> expect_equal(0.145)
  # `"up_from"`'s lower bound is `threshold - 10` units of 1/10^(digits + 1):
  expect_equal(
    unround("0.53", rounding = "up_from", threshold = 4.5)$lower,
    0.53 - 0.0055
  )
  # A threshold of 5 is a valid specification, not a missing one. It used to
  # throw an error, on the theory that it could only be the argument's default
  # value showing through -- so any caller that computed a threshold failed
  # spuriously at exactly the most common value:
  2.345 |> reround(2, "up_from", threshold = 5) |> expect_equal(2.35)
})


# Exact decimal boundaries ------------------------------------------------

# Shifting a number by `digits` decimal places is inexact: `0.28 * 100` is
# 28.000000000000004, and `0.29 * 100` is 28.999999999999996. Rounding the
# shifted value away from the number it stands for would move it a whole step.

test_that("`round_ceiling()` and `round_floor()` are exact at whole steps", {
  x <- seq(0, 1000) / 100

  x |> round_ceiling(2) |> expect_equal(x)
  x |> round_floor(2)   |> expect_equal(x)
  x |> round_trunc(2)   |> expect_equal(x)
  -x |> round_trunc(2) |> expect_equal(-x)

  # `round_anti_trunc()` leaves a value that is already on the grid where it is,
  # and takes anything else to the next step away from zero:
  x |> round_anti_trunc(2) |> expect_equal(x)
  -x |> round_anti_trunc(2) |> expect_equal(-x)
  expect_equal(round_anti_trunc(x[-1] - 0.005, 2), x[-1])
  expect_equal(round_anti_trunc(-(x[-1] - 0.005), 2), -x[-1])

  # The individual cases that used to fail:
  0.28 |> round_ceiling(2) |> expect_equal(0.28)
  0.29 |> round_floor(2) |> expect_equal(0.29)

  # Values genuinely inside a step still move to its edge:
  0.281 |> round_ceiling(2) |> expect_equal(0.29)
  0.289 |> round_floor(2) |> expect_equal(0.28)
})

test_that("all rounding functions share the same tolerance", {
  # `round_up_from()` and `round_down_from()` fold `ROUNDING_TOLERANCE` into
  # `tie_offset()`, the others add or subtract it directly. The equality was
  # implicit until scrutiny 1.0.0, when the `*_from()` functions expressed it as
  # `threshold - .Machine$double.eps^0.5` instead -- which the `/ 10` in the
  # formula turns into the very same additive nudge. `unround()`'s bounds assume
  # one shared tolerance, so the two families must not drift apart:
  x <- seq(0, 1000) / 100

  # Ties are ties whichever way the shifted value happens to be represented:
  expect_equal(round_up(x + 0.005, 2), round_ceiling(x + 0.001, 2))
  expect_equal(round_down(x + 0.005, 2), round_floor(x + 0.001, 2))

  # The one case in which the tolerance is visible at all, at both families:
  0.145 |> round_up(2)   |> expect_equal(0.15)
  0.145 |> round_down(2) |> expect_equal(0.14)
  expect_equal(round_ceiling(0.145 - 0.005, 2), 0.14)
})


# Tie procedures named directly -------------------------------------------

# `round_ties_*()` are the four combinations of `round_up()` / `round_down()`
# with `symmetric`, under names that say which procedure they are, plus
# `round_ties_even()`, which is `base::round()` and has no such second spelling.
# The point of having them is that the translation is easy to get backwards, so
# the tests below pin both halves: the equivalence, and the behavior itself.

test_that("`round_ties_*()` are the `symmetric` combinations of up and down", {
  x <- c(mid, -mid, seq(-999, 999) / 100, seq(-20, 20) / 8)
  for (digits in 0:2) {
    x |> round_ties_up(digits)   |> expect_equal(round_up(x, digits, symmetric = FALSE))
    x |> round_ties_down(digits) |> expect_equal(round_down(x, digits, symmetric = FALSE))
    x |> round_ties_away(digits) |> expect_equal(round_up(x, digits, symmetric = TRUE))
    x |> round_ties_zero(digits) |> expect_equal(round_down(x, digits, symmetric = TRUE))
  }
})

test_that("`round_ties_*()` break ties as their names say", {
  # Hand-computed, not taken from any scrutiny function:
  c(-2.5, -0.5, 0.5, 2.5) |> round_ties_up()   |> expect_equal(c(-2, 0, 1, 3))
  c(-2.5, -0.5, 0.5, 2.5) |> round_ties_down() |> expect_equal(c(-3, -1, 0, 2))
  c(-2.5, -0.5, 0.5, 2.5) |> round_ties_away() |> expect_equal(c(-3, -1, 1, 3))
  c(-2.5, -0.5, 0.5, 2.5) |> round_ties_zero() |> expect_equal(c(-2, 0, 0, 2))

  # Above zero, the two pairs collapse into each other:
  mid |> round_ties_up(1)   |> expect_equal(round_ties_away(mid, 1))
  mid |> round_ties_down(1) |> expect_equal(round_ties_zero(mid, 1))
})

test_that("the `\"ties_*\"` strings mean the same as the functions", {
  x <- c(mid, -mid, seq(-500, 500) / 100)
  x |> reround(1, "ties_up")   |> expect_equal(round_ties_up(x, 1))
  x |> reround(1, "ties_down") |> expect_equal(round_ties_down(x, 1))
  x |> reround(1, "ties_away") |> expect_equal(round_ties_away(x, 1))
  x |> reround(1, "ties_zero") |> expect_equal(round_ties_zero(x, 1))
  x |> reround(1, "ties_even") |> expect_equal(round_ties_even(x, 1))
})

# `round_ties_even()` completes the family. *roundTiesToEven* is IEEE 754's
# default direction and the one R, Python, and NumPy take, so it was the
# conspicuous absence from a set of names whose whole point is naming the tie
# rule -- reachable only as `rounding = "even"`, which does not look like a
# member of the family it belongs to.

test_that("`round_ties_even()` is `base::round()` under the family's name", {
  x <- c(mid, -mid, seq(-999, 999) / 100, seq(-20, 20) / 8)
  for (digits in 0:2) {
    x |> round_ties_even(digits) |> expect_equal(round(x, digits))
  }
  # The tie itself, at the one place where the decimal really is a tie in
  # binary, so that parity decides it and the result can be written down:
  c(-2.5, -0.5, 0.5, 2.5) |> round_ties_even() |> expect_equal(c(-2, 0, 0, 2))
})

test_that("`\"ties_even\"` and `\"even\"` are the same rounding method", {
  x <- c(mid, -mid, seq(-500, 500) / 100)
  for (digits in 0:2) {
    x |> reround(digits, "ties_even") |> expect_equal(reround(digits = digits, x = x, rounding = "even"))
  }
  # ...including in the bounds, which is what a consistency test reads:
  from_even <- unround("0.53", rounding = "even")
  from_ties <- unround("0.53", rounding = "ties_even")
  expect_equal(from_even$lower, from_ties$lower)
  expect_equal(from_even$upper, from_ties$upper)
  expect_equal(from_even$incl_lower, from_ties$incl_lower)
  expect_equal(from_even$incl_upper, from_ties$incl_upper)
})

# Each `"ties_*"` string names a complete procedure, so a separate argument must
# not be able to turn it into a different one. Up to scrutiny 1.0.0 that was
# enforced by ignoring `symmetric`, which kept the name authoritative but threw
# away half of what the caller wrote -- and did so in the one case where they
# were most likely to mean it, since `symmetric` is exactly the argument they
# were told controls tie direction for negative numbers. Rejecting the
# combination keeps the name authoritative just as well, and says which string
# they meant.

test_that("`symmetric` is rejected for the `\"ties_*\"` strings", {
  for (rounding in names(TIES_METHODS)) {
    -2.5 |>
      reround(0, rounding, symmetric = TRUE) |>
      expect_error("must not be given")
    # The bounds side resolves the same strings through the same helper, so it
    # has to agree -- including via a mapper, which is where a silently dropped
    # `symmetric` used to reach a verdict:
    "-2.5" |>
      unround(rounding = rounding, digits = 1, symmetric = TRUE) |>
      expect_error("must not be given")
    tibble::tibble(x = -5.19, n = 28) |>
      grim_map(digits_x = 2, rounding = rounding, symmetric = TRUE) |>
      expect_error("must not be given")
  }
})

test_that("the default `symmetric` still passes, and names the alternative", {
  # `FALSE` is the default, so it is the only thing that can mean "not given".
  # Passing it explicitly must not error:
  x <- c(mid, -mid)
  x |> reround(1, "ties_away", symmetric = FALSE) |> expect_equal(round_ties_away(x, 1))
  x |> reround(1, "ties_up", symmetric = FALSE)   |> expect_equal(round_ties_up(x, 1))

  # The mirrored counterpart is named where there is one:
  -2.5 |> reround(0, "ties_up", symmetric = TRUE)   |> expect_error("ties_away")
  -2.5 |> reround(0, "ties_down", symmetric = TRUE) |> expect_error("ties_zero")

  # ...and it is the string that does what the rejected combination looked like
  # it was asking for. Hand-computed:
  reround(-2.5, 0, "ties_up")   |> expect_equal(-2)
  reround(-2.5, 0, "ties_away") |> expect_equal(-3)
  reround(-2.5, 0, "up", symmetric = TRUE) |> expect_equal(-3)
})

test_that("`reround()` takes one rounding procedure, not a vector of them", {
  # Vectorized `rounding`, `threshold`, and `symmetric` are gone: they describe
  # a single procedure, and `x` is the vector. `unround()` keeps the behavior
  # for its display use case.
  c(1.5, 2.5) |> reround(0, c("up", "down")) |> expect_error()
  1.5 |> reround(0, "up_from", threshold = c(3, 7)) |> expect_error()
  1.5 |> reround(0, "up", symmetric = c(TRUE, FALSE)) |> expect_error()
  # (`unround()` warns about the pairing, which is the documented behavior.)
  c("1.5", "2.5") |>
    unround(rounding = c("up", "down")) |>
    suppressWarnings() |>
    suppressMessages() |>
    nrow() |>
    expect_equal(2L)
})


test_that("`round_up_from()` and `round_down_from()` validate `threshold`", {
  # These are the functions that act on `threshold`, and a value outside of `(0,
  # 10)` silently turns them into `round_ceiling()` or `round_floor()`.
  # `reround()` and `unround()` have always checked; these two did not.
  4.28 |> round_up_from(1, threshold = 0)    |> expect_error("threshold")
  4.28 |> round_up_from(1, threshold = 10)   |> expect_error("threshold")
  4.28 |> round_up_from(1, threshold = -3)   |> expect_error("threshold")
  4.28 |> round_down_from(1, threshold = 0)  |> expect_error("threshold")
  4.28 |> round_down_from(1, threshold = 10) |> expect_error("threshold")
  # Valid thresholds still work, and still agree with `reround()`:
  4.28 |> round_up_from(1, threshold = 9) |> expect_equal(4.2)
  4.28 |> round_up_from(1, threshold = 1) |> expect_equal(4.3)
  4.28 |> round_up(1) |> expect_equal(round_up_from(4.28, 1, threshold = 5))
})
