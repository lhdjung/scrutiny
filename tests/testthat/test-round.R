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
  expect_equal(round_up(mid, 1), (seq(0, 9) + 1) / 10)
})

test_that("`round_down()` sends positive ties to the lower neighbor", {
  # 0.05 -> 0.0, 0.15 -> 0.1, ..., 0.95 -> 0.9
  expect_equal(round_down(mid, 1), seq(0, 9) / 10)
})

test_that("`round_up()` sends ties toward `+Inf` by default", {
  # This is the default that `vignette("rounding-options")` now warns about: on
  # the number line, a negative tie goes up just like a positive one, so -0.05
  # -> 0.0 and -0.95 -> -0.9. Excel, SAS, and Matlab do *not* do this; see the
  # test after the next one.
  expect_equal(round_up(-mid, 1), -seq(0, 9) / 10)
})

test_that("`round_down()` sends ties toward `-Inf` by default", {
  expect_equal(round_down(-mid, 1), -(seq(0, 9) + 1) / 10)
})

test_that("`symmetric = TRUE` mirrors ties around zero", {
  # This is IEEE 754's *roundTiesToAway*, i.e. what Excel's `ROUND()`, SAS's
  # `ROUND()`, Matlab's `round()`, and `janitor::round_half_up()` do:
  expect_equal(round_up(-mid, 1, symmetric = TRUE), -(seq(0, 9) + 1) / 10)
  # ...and its mirror image, ties toward zero:
  expect_equal(round_down(-mid, 1, symmetric = TRUE), -seq(0, 9) / 10)
  # For non-negative numbers, `symmetric` changes nothing:
  expect_equal(round_up(mid, 1, symmetric = TRUE), round_up(mid, 1))
  expect_equal(round_down(mid, 1, symmetric = TRUE), round_down(mid, 1))
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
    expect_equal(round_up(non_ties, 1, symmetric), round(non_ties, 1))
    expect_equal(round_down(non_ties, 1, symmetric), round(non_ties, 1))
  }
})


# Custom thresholds -------------------------------------------------------

# `round_up_from()` rounds up whenever the part cut off by rounding is at least
# `threshold` tenths of a step. `round_down_from()` is its mirror image, and the
# mirroring applies to `threshold` as well: it rounds *down* whenever the
# cut-off part is at most `10 - threshold` tenths of a step. The two coincide
# only at the threshold of 5 that `round_up()` and `round_down()` use, which is
# why nothing so far has depended on the difference.
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

test_that("`round_down_from()` rounds down from `10 - threshold`", {
  for (threshold in seq(1, 9)) {
    for (base in c(0.42, 7.13, 100)) {
      expect_equal(
        round_down_from(base + cut_digit / 1000, 2, threshold = threshold),
        base + ifelse(cut_digit > 10 - threshold, 0.01, 0),
        label = paste("`round_down_from()`, threshold", threshold, "at", base)
      )
    }
  }
})

test_that("`round_down_from()` is `round_up_from()` with the tie reversed", {
  # The precise sense in which the two are "mirror images": they switch
  # direction at the same point, `10 - threshold`, and differ only in which way
  # a value sitting exactly on that point goes. Exactly on the three-decimal
  # grid, so that the cut-off part really is the third decimal and `on_tie`
  # below identifies it without any rounding of its own:
  x <- seq(0, 9999) / 1000
  for (threshold in seq(1, 9)) {
    down <- round_down_from(x, 2, threshold = threshold)
    up <- round_up_from(x, 2, threshold = 10 - threshold)
    on_tie <- round(x * 1000) %% 10 == 10 - threshold
    expect_equal(down[!on_tie], up[!on_tie])
    # On the tie itself, `"down_from"` goes down and `"up_from"` goes up:
    expect_equal(down[on_tie] + 0.01, up[on_tie])
  }
})

test_that("a `threshold` of 5 makes the `*_from()` functions the plain ones", {
  x <- c(mid, -mid, seq(-500, 500) / 100)
  expect_equal(round_up_from(x, 1, threshold = 5), round_up(x, 1))
  expect_equal(round_down_from(x, 1, threshold = 5), round_down(x, 1))
  expect_equal(
    round_up_from(x, 1, threshold = 5, symmetric = TRUE),
    round_up(x, 1, symmetric = TRUE)
  )
  expect_equal(
    round_down_from(x, 1, threshold = 5, symmetric = TRUE),
    round_down(x, 1, symmetric = TRUE)
  )
})

test_that("`threshold` is rejected outside of the interval it has to lie in", {
  # At 0 or 10, one of the two directions can never be taken, which would
  # silently turn the method into a ceiling or a floor:
  expect_error(reround(0.5, 1, "up_from", threshold = 0))
  expect_error(reround(0.5, 1, "up_from", threshold = 10))
  expect_error(reround(0.5, 1, "down_from", threshold = -2))
  expect_error(reround(0.5, 1, "up_from_or_down_from", threshold = NA))
  expect_error(unround("0.53", rounding = "up_from", threshold = 12))
  # ...whereas a fractional threshold inside it is fine. `unround()` scales the
  # bounds up until they are whole numbers again, so this also exercises the
  # rescaling loop in `bound_numerators()`:
  expect_equal(reround(0.1445, 3, "up_from", threshold = 4.5), 0.145)
  # `"up_from"`'s lower bound is `threshold - 10` units of 1/10^(digits + 1):
  expect_equal(
    unround("0.53", rounding = "up_from", threshold = 4.5)$lower,
    0.53 - 0.0055
  )
  # A threshold of 5 is a valid specification, not a missing one. It used to
  # throw an error, on the theory that it could only be the argument's default
  # value showing through -- so any caller that computed a threshold failed
  # spuriously at exactly the most common value:
  expect_equal(reround(2.345, 2, "up_from", threshold = 5), 2.35)
})


# Exact decimal boundaries ------------------------------------------------

# Shifting a number by `digits` decimal places is inexact: `0.28 * 100` is
# 28.000000000000004, and `0.29 * 100` is 28.999999999999996. Rounding the
# shifted value away from the number it stands for would move it a whole step.

test_that("`round_ceiling()` and `round_floor()` are exact at whole steps", {
  x <- seq(0, 1000) / 100

  expect_equal(round_ceiling(x, 2), x)
  expect_equal(round_floor(x, 2), x)
  expect_equal(round_trunc(x, 2), x)
  expect_equal(round_trunc(-x, 2), -x)

  # `round_anti_trunc()` leaves a value that is already on the grid where it is,
  # and takes anything else to the next step away from zero:
  expect_equal(round_anti_trunc(x, 2), x)
  expect_equal(round_anti_trunc(-x, 2), -x)
  expect_equal(round_anti_trunc(x[-1] - 0.005, 2), x[-1])
  expect_equal(round_anti_trunc(-(x[-1] - 0.005), 2), -x[-1])

  # The individual cases that used to fail:
  expect_equal(round_ceiling(0.28, 2), 0.28)
  expect_equal(round_floor(0.29, 2), 0.29)

  # Values genuinely inside a step still move to its edge:
  expect_equal(round_ceiling(0.281, 2), 0.29)
  expect_equal(round_floor(0.289, 2), 0.28)
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
  expect_equal(round_up(0.145, 2), 0.15)
  expect_equal(round_down(0.145, 2), 0.14)
  expect_equal(round_ceiling(0.145 - 0.005, 2), 0.14)
})


# Tie procedures named directly -------------------------------------------

# `round_ties_*()` are the four combinations of `round_up()` / `round_down()`
# with `symmetric`, under names that say which procedure they are. The point of
# having them is that the translation is easy to get backwards, so the tests
# below pin both halves: the equivalence, and the behavior itself.

test_that("`round_ties_*()` are the `symmetric` combinations of up and down", {
  x <- c(mid, -mid, seq(-999, 999) / 100, seq(-20, 20) / 8)
  for (digits in 0:2) {
    expect_equal(
      round_ties_up(x, digits),
      round_up(x, digits, symmetric = FALSE)
    )
    expect_equal(
      round_ties_down(x, digits),
      round_down(x, digits, symmetric = FALSE)
    )
    expect_equal(
      round_ties_away(x, digits),
      round_up(x, digits, symmetric = TRUE)
    )
    expect_equal(
      round_ties_zero(x, digits),
      round_down(x, digits, symmetric = TRUE)
    )
  }
})

test_that("`round_ties_*()` break ties as their names say", {
  # Hand-computed, not taken from any scrutiny function:
  expect_equal(round_ties_up(c(-2.5, -0.5, 0.5, 2.5)), c(-2, 0, 1, 3))
  expect_equal(round_ties_down(c(-2.5, -0.5, 0.5, 2.5)), c(-3, -1, 0, 2))
  expect_equal(round_ties_away(c(-2.5, -0.5, 0.5, 2.5)), c(-3, -1, 1, 3))
  expect_equal(round_ties_zero(c(-2.5, -0.5, 0.5, 2.5)), c(-2, 0, 0, 2))

  # Above zero, the two pairs collapse into each other:
  expect_equal(round_ties_up(mid, 1), round_ties_away(mid, 1))
  expect_equal(round_ties_down(mid, 1), round_ties_zero(mid, 1))
})

test_that("the `\"ties_*\"` strings mean the same as the functions", {
  x <- c(mid, -mid, seq(-500, 500) / 100)
  expect_equal(reround(x, 1, "ties_up"), round_ties_up(x, 1))
  expect_equal(reround(x, 1, "ties_down"), round_ties_down(x, 1))
  expect_equal(reround(x, 1, "ties_away"), round_ties_away(x, 1))
  expect_equal(reround(x, 1, "ties_zero"), round_ties_zero(x, 1))
})

test_that("`symmetric` is ignored for the `\"ties_*\"` strings", {
  # Each of them names a complete procedure, so a separate argument must not be
  # able to turn it into a different one:
  x <- c(mid, -mid)
  for (symmetric in c(FALSE, TRUE)) {
    expect_equal(
      reround(x, 1, "ties_away", symmetric = symmetric),
      round_ties_away(x, 1)
    )
    expect_equal(
      reround(x, 1, "ties_up", symmetric = symmetric),
      round_ties_up(x, 1)
    )
  }
})

test_that("`reround()` takes one rounding procedure, not a vector of them", {
  # Vectorized `rounding`, `threshold`, and `symmetric` are gone: they describe
  # a single procedure, and `x` is the vector. `unround()` keeps the behavior
  # for its display use case.
  expect_error(reround(c(1.5, 2.5), 0, c("up", "down")))
  expect_error(reround(1.5, 0, "up_from", threshold = c(3, 7)))
  expect_error(reround(1.5, 0, "up", symmetric = c(TRUE, FALSE)))
  # (`unround()` warns about the pairing, which is the documented behavior.)
  unround(c("1.5", "2.5"), rounding = c("up", "down")) |>
    suppressWarnings() |>
    suppressMessages() |>
    nrow() |>
    expect_equal(2L)
})


test_that("`round_up_from()` and `round_down_from()` validate `threshold`", {
  # These are the functions that act on `threshold`, and a value outside of
  # `(0, 10)` silently turns them into `round_ceiling()` or `round_floor()`.
  # `reround()` and `unround()` have always checked; these two did not.
  round_up_from(4.28, 1, threshold = 0) |> expect_error("threshold")
  round_up_from(4.28, 1, threshold = 10) |> expect_error("threshold")
  round_up_from(4.28, 1, threshold = -3) |> expect_error("threshold")
  round_down_from(4.28, 1, threshold = 0) |> expect_error("threshold")
  round_down_from(4.28, 1, threshold = 10) |> expect_error("threshold")
  # Valid thresholds still work, and still agree with `reround()`:
  round_up_from(4.28, 1, threshold = 9) |> expect_equal(4.2)
  round_up_from(4.28, 1, threshold = 1) |> expect_equal(4.3)
  round_up(4.28, 1) |> expect_equal(round_up_from(4.28, 1, threshold = 5))
})
