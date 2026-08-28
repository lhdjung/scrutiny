df1 <- unround(c(3.6, "5.20", 5.174)) |>
  suppressMessages()


test_that("The output is a tibble", {
  df1 |> expect_s3_class("tbl_df")
})

test_that("It has correct dimensions", {
  df1 |> dim() |> expect_equal(c(3, 7))
})


colnames_expected <- c(
  "range",
  "rounding",
  "lower",
  "incl_lower",
  "x",
  "incl_upper",
  "upper"
)

test_that("It has correct column names", {
  df1 |> colnames() |> expect_setequal(colnames_expected)
})


test_that("Its columns have the correct types", {
  df1[[1]] |> expect_type("character")
  df1[[2]] |> expect_type("character")
  df1[[3]] |> expect_type("double")
  df1[[4]] |> expect_type("logical")
  df1[[5]] |> expect_type("character")
  df1[[6]] |> expect_type("logical")
  df1[[7]] |> expect_type("double")
})


test_that("A non-string `x` specification throws an error
          (if `digits` is `NULL`; the default)", {
  4.5 |> unround() |> expect_error()
})

test_that("The function throws an error if `rounding` is misspecified", {
  "4.50" |> unround(rounding = "doesn't exist") |> expect_error()
})


df2 <- unround("4.50", digits = 1)
df3 <- unround("4.50", digits = 1:5)


test_that("", {
  df2$lower |> expect_equal(4.45)
  df2$upper |> expect_equal(4.55)
})


test_that("", {
  df3$lower |> expect_equal(c(4.450000, 4.495000, 4.499500, 4.499950, 4.499995))
  df3$upper |> expect_equal(c(4.550000, 4.505000, 4.500500, 4.500050, 4.500005))
})


test_that("A vector-valued `digits` yields a well-formed tibble", {
  # The row count used to be taken from `length(x)` while the columns took
  # whatever length `paste0()` recycling produced, so this was a tibble
  # claiming one row but holding five-element columns.
  df3 |> nrow() |> expect_equal(5L)
  df3 |> vapply(length, integer(1L)) |> unname() |> expect_equal(rep(5L, 7L))
  df3 |> as.data.frame() |> nrow() |> expect_equal(5L)
})


# `unround()` is the inverse of `reround()`: a value just inside the
# reconstructed range must round back to `x`, and a value just outside it must
# not. This is the property that matters, and it ties the bounds to the
# rounding functions they claim to invert.
test_that("`unround()` bounds agree with the rounding they invert", {
  # `eps` is far below the granularity of the bounds (0.001 here) but far above
  # the dust that `round_up_from()` and friends subtract from `threshold`:
  eps <- 1e-6
  methods <- c(
    "up_or_down", "up", "down", "ceiling", "floor",
    "trunc", "anti_trunc", "up_from", "down_from"
  )
  for (m in methods) {
    bounds <- unround("0.53", rounding = m, threshold = 6)
    rounds_to_x <- function(value) {
      value |>
        reround(digits = 2, rounding = m, threshold = 6) |>
        dplyr::near(0.53) |>
        any()
    }
    expect_true(rounds_to_x(bounds$lower + eps), label = paste(m, "inside lower"))
    expect_true(rounds_to_x(bounds$upper - eps), label = paste(m, "inside upper"))
    expect_false(rounds_to_x(bounds$lower - eps), label = paste(m, "beyond lower"))
    expect_false(rounds_to_x(bounds$upper + eps), label = paste(m, "beyond upper"))
  }
})


# The same property, swept across the whole input space instead of the single
# point above.
#
# The package holds two independent encodings of every rounding procedure: the
# forward `round_*()` functions dispatched by `reround()`, which `grimmer()` and
# `debit()` still run on directly, and the inverse `rounding_offsets()` /
# `bound_numerators()` in unround.R, which is what GRIM, GRIMMER, and DEBIT
# derive their candidate ranges from. If the two ever drift apart -- an offset
# table edit, a tolerance change in a forward function -- a consistency test
# would compare a value rounded by one convention against bounds derived from
# the other, and verdicts could flip with nothing to catch it.
#
# The sweep covers every rounding method, the sign of `x`, `symmetric`, several
# decimal counts, and several thresholds, and it checks the *inclusivity* of
# each bound as well as its position: a value exactly on an inclusive bound must
# round back to `x`, and one exactly on an exclusive bound must not. That last
# pair is what the tolerance in the forward functions exists for, so it also
# pins the tolerance to the bounds.

test_that("`unround()` bounds agree with the rounding they invert (sweep)", {
  methods <- c(
    "up_or_down", "up", "down", "even", "ceiling", "floor", "ceiling_or_floor",
    "trunc", "anti_trunc", "up_from", "down_from", "up_from_or_down_from",
    "ties_up", "ties_down", "ties_away", "ties_zero"
  )
  # `"even"` is the one method whose bounds cannot be pinned down, since
  # `base::round()` breaks ties by the parity of the binary double. Both of its
  # bounds are deliberately reported as inclusive, which can only widen the
  # range, so only the "inside rounds back" half of the property applies to it:
  methods_exact <- setdiff(methods, "even")

  n_checked <- 0L

  for (digits in c(0L, 1L, 2L)) {
    unit <- 10^-digits
    # A grid step is `unit`, and the bounds sit on the `unit / 10` grid, so this
    # is far below any bound spacing and far above `ROUNDING_TOLERANCE`:
    eps <- unit / 1000

    for (x_num in c(0, 1, -1, 3, -3, 253, -253) * unit) {
      x_str <- formatC(x_num, format = "f", digits = digits)
      for (symmetric in c(FALSE, TRUE)) {
        for (threshold in c(3, 5, 6)) {
          for (m in methods) {
            bounds <- unround(
              x_str,
              rounding = m,
              threshold = threshold,
              digits = digits,
              symmetric = symmetric
            )
            rounds_to_x <- function(value) {
              value |>
                reround(
                  digits = digits,
                  rounding = m,
                  threshold = threshold,
                  symmetric = symmetric
                ) |>
                dplyr::near(x_num) |>
                any()
            }
            label <- paste(
              m, "| x =", x_str, "| digits =", digits,
              "| symmetric =", symmetric, "| threshold =", threshold
            )
            n_checked <- n_checked + 1L

            # `"anti_trunc"` at zero is the one degenerate range: every non-zero
            # value is taken away from zero, so the only value reported as zero
            # is zero itself. There is no "just inside" to check, and both
            # bounds are the point itself:
            if (bounds$lower == bounds$upper) {
              expect_equal(m, "anti_trunc")
              expect_equal(x_num, 0)
              expect_true(rounds_to_x(x_num), label = paste(label, "- at point"))
              expect_true(bounds$incl_lower && bounds$incl_upper)
              expect_false(
                rounds_to_x(x_num + eps),
                label = paste(label, "- beyond upper")
              )
              expect_false(
                rounds_to_x(x_num - eps),
                label = paste(label, "- beyond lower")
              )
              next
            }

            # Position of the bounds:
            expect_true(
              rounds_to_x(bounds$lower + eps),
              label = paste(label, "- inside lower")
            )
            expect_true(
              rounds_to_x(bounds$upper - eps),
              label = paste(label, "- inside upper")
            )
            expect_false(
              rounds_to_x(bounds$lower - eps),
              label = paste(label, "- beyond lower")
            )
            expect_false(
              rounds_to_x(bounds$upper + eps),
              label = paste(label, "- beyond upper")
            )

            # Inclusivity of the bounds:
            if (m %in% methods_exact) {
              expect_equal(
                rounds_to_x(bounds$lower),
                bounds$incl_lower,
                label = paste(label, "- on lower")
              )
              expect_equal(
                rounds_to_x(bounds$upper),
                bounds$incl_upper,
                label = paste(label, "- on upper")
              )
            }
          }
        }
      }
    }
  }

  # Guard against the loops silently collapsing to nothing:
  expect_gt(n_checked, 700L)
})


test_that("`unround()` supports the same rounding methods as GRIM", {
  # These four used to throw an error, so `debit_map()` rejected rounding
  # methods that `grim()` accepted.
  for (m in c(
    "up_from", "down_from", "up_from_or_down_from", "ceiling_or_floor"
  )) {
    "0.53" |>
      unround(rounding = m, threshold = 6) |>
      nrow() |>
      expect_equal(1L)
  }
})


test_that("`threshold` only affects the `*_from` rounding methods", {
  # `round_up()` and `round_down()` round from a fixed 5, so reconstructing
  # their bounds must not depend on `threshold`. `unround()` used to widen the
  # range anyway, reconstructing a rounding that never happens.
  for (m in c("up_or_down", "up", "down")) {
    from_5 <- unround("0.53", rounding = m, threshold = 5)
    from_6 <- unround("0.53", rounding = m, threshold = 6)
    expect_equal(from_5$lower, from_6$lower)
    expect_equal(from_5$upper, from_6$upper)
    expect_equal(from_5$lower, 0.525)
    expect_equal(from_5$upper, 0.535)
  }
  # ...whereas the parameterized methods do respond to it:
  unround("0.53", rounding = "up_from", threshold = 6)$lower |>
    expect_equal(0.526)
})


test_that("`symmetric` mirrors the bounds of a negative `x`", {
  plain <- unround("-0.53", rounding = "up", symmetric = FALSE)
  mirrored <- unround("-0.53", rounding = "up", symmetric = TRUE)
  # With `symmetric`, rounding a negative number mirrors its absolute value,
  # so the inclusive end swaps:
  expect_true(plain$incl_lower)
  expect_false(plain$incl_upper)
  expect_false(mirrored$incl_lower)
  expect_true(mirrored$incl_upper)
})


test_that("`\"anti_trunc\"` bounds match `round_anti_trunc()`", {
  # `round_anti_trunc()` rounds away from zero, leaving a value that already
  # sits on the rounding grid where it is. So it is `round_ceiling()` above
  # zero, which reaches `x` from below and includes `x` itself...
  # (`x` is given with one decimal place, and re-rounded to one, so that the
  # bounds and the rounding are on the same grid. It used to be given as "0.70",
  # i.e. unrounded at two decimal places and then re-rounded at one.)
  bounds_positive <- unround("0.7", rounding = "anti_trunc")
  expect_false(bounds_positive$incl_lower)
  expect_true(bounds_positive$incl_upper)
  expect_equal(round_anti_trunc(bounds_positive$upper, 1), 0.7)
  expect_false(round_anti_trunc(bounds_positive$lower, 1) == 0.7)

  # ...and `round_floor()` below zero, which reaches it from above:
  bounds_negative <- unround("-0.7", rounding = "anti_trunc")
  expect_true(bounds_negative$incl_lower)
  expect_false(bounds_negative$incl_upper)
  expect_equal(round_anti_trunc(bounds_negative$lower, 1), -0.7)
  expect_false(round_anti_trunc(bounds_negative$upper, 1) == -0.7)
})


test_that("`\"anti_trunc\"` at zero is a single point, not an undefined range", {
  # Every non-zero value is taken away from zero to the next step out, so the
  # only value that would be reported as zero is zero itself. Up to scrutiny
  # 1.0.0, `anti_trunc()` sent zero itself away from zero as well -- to `+1`
  # unit, an arbitrary sign choice -- and the bounds here were `NA` in
  # consequence.
  bounds <- unround("0.00", rounding = "anti_trunc")
  expect_equal(bounds$lower, 0)
  expect_equal(bounds$upper, 0)
  expect_true(bounds$incl_lower)
  expect_true(bounds$incl_upper)
  expect_equal(round_anti_trunc(0, 2), 0)
  expect_false(round_anti_trunc(0.001, 2) == 0)
  expect_false(round_anti_trunc(-0.001, 2) == 0)

  # It follows that a mean of zero pins the sum to exactly zero, so GRIM is
  # decidable there rather than `NA`, and consistent only for all-zero data:
  expect_true(grim(0, n = 40, digits_x = 2, rounding = "anti_trunc"))
  expect_false(grim(0.01, n = 40, digits_x = 2, rounding = "anti_trunc"))
})


test_that("`unround()` checks the lengths of all its vectorized arguments", {
  # `digits` used to be recycled against `x` without a word, so the extra `x`
  # values silently got the wrong number of decimal places -- and hence the
  # wrong bounds.
  # Not aligning pipes here because the lengths are too different
  c("1.0", "2.00", "3.000") |> unround(digits = c(1, 2)) |> expect_error()
  c("1.0", "2.00") |> unround(threshold = c(4, 5, 6)) |> expect_error()
  c("1.0", "2.00") |> unround(symmetric = c(TRUE, FALSE, TRUE)) |> expect_error()
  # A length-1 argument is still recycled, as is one of matching length:
  c("1.0", "2.00") |> unround(digits = c(1, 2)) |> expect_no_error()
  c("1.0", "2.00") |> unround() |> expect_no_error()
})


test_that("counted decimal places are not confused by a shorter `digits`", {
  out <- unround(c("1.0", "2.00", "3.000"))
  out$upper |> expect_equal(c(1.05, 2.005, 3.0005))
})


test_that("zero-length input returns zero rows", {
  # Recycling stops at zero. Taking the maximum of all argument lengths let the
  # length-1 defaults set the row count, so an empty `x` produced one row of
  # missing values -- a phantom result in the middle of a pipeline.
  character(0) |> unround() |> nrow() |> expect_equal(0L)
  numeric(0) |> unround(digits = 2) |> nrow() |> expect_equal(0L)
  "5.19" |> unround(digits = integer(0)) |> nrow() |> expect_equal(0L)
  # A scalar `x` is unaffected:
  "5.19" |> unround() |> nrow() |> expect_equal(1L)
})


# Width of a reconstructed range -------------------------------------------

# The sweep above pins the bounds to the rounding functions, but it can only
# catch a disagreement between the two. It cannot catch the two agreeing on
# something impossible, which is what `rounding = "up_from_or_down_from"` did up
# to scrutiny 1.0.0: `round_down_from()` switched direction at `10 - threshold`
# rather than at `threshold`, so the compound method's range was the union of
# two intervals that barely overlapped, and it spanned up to 1.8 steps.
#
# The invariant that catches it: a rounding procedure maps every value to the
# nearest point of the grid that its own tie rule picks out, so the set of
# values that reach any one point is at most one step wide.
#
# Zero is where that stops holding, and legitimately so. A procedure whose
# behavior depends on the sign takes one side of zero by one rule and the other
# by the mirror-image rule, so the bin at zero is the union of two half-bins
# that need not add up to a step:
#
#   - `"trunc"` at zero accepts all of `(-1, 1)`: every value between truncates
#     to `0`. Two steps. Away from zero it takes one side alone and is one step
#     wide like everything else.
#   - `symmetric` mirroring at zero gives `2 * threshold / 10` steps, since a
#     value reaches zero exactly if its absolute value does. That is one step at
#     the `5` of `"up"`, `"down"`, and `"up_or_down"` -- which is why only the
#     `"*_from"` methods show it -- and anything from 0.2 to 1.8 steps
#     otherwise.
#
# `"ceiling_or_floor"` is two steps wide everywhere, not just at zero: its two
# constituents lie on opposite sides of `x` at every value. All of these ranges
# are open at both ends, since a value sitting on either bound is carried away
# from `x`.
#
# The test below therefore checks the one-step bound everywhere it applies and
# pins the exceptions to their exact widths.

test_that("no rounding method reconstructs a range wider than one step", {
  methods_one_step <- c(
    "up_or_down", "up", "down", "even", "ceiling", "floor", "trunc",
    "anti_trunc", "up_from", "down_from", "up_from_or_down_from",
    "ties_up", "ties_down", "ties_away", "ties_zero", "ties_even"
  )

  for (digits in c(0L, 1L, 2L)) {
    unit <- 10^-digits
    for (x_num in c(0, 1, -1, 253, -253) * unit) {
      x_str <- formatC(x_num, format = "f", digits = digits)
      for (symmetric in c(FALSE, TRUE)) {
        for (threshold in seq(1, 9)) {
          # The methods whose bins at zero are two half-bins joined; see the
          # comment above this test:
          mirrored <- c(
            "up_or_down", "up", "down",
            "up_from_or_down_from", "up_from", "down_from"
          )

          for (m in methods_one_step) {
            if (x_num == 0 && (m == "trunc" || (symmetric && m %in% mirrored))) {
              next
            }
            bounds <- unround(
              x_str,
              rounding = m,
              threshold = threshold,
              digits = digits,
              symmetric = symmetric
            )
            expect_lte(
              bounds$upper - bounds$lower,
              unit * (1 + 1e-9),
              label = paste(
                m, "| x =", x_str, "| digits =", digits,
                "| symmetric =", symmetric, "| threshold =", threshold
              )
            )
          }
          # Two steps wide, open at both ends:
          two_sided <- "ceiling_or_floor"
          if (x_num == 0) {
            two_sided <- c(two_sided, "trunc")
          }
          for (m in two_sided) {
            wide <- unround(
              x_str,
              rounding = m,
              threshold = threshold,
              digits = digits
            )
            expect_equal(wide$upper - wide$lower, 2 * unit)
            expect_false(wide$incl_lower)
            expect_false(wide$incl_upper)
          }

          # `symmetric` at zero: `2 * threshold / 10` steps, open at both ends,
          # with the `"*_from"` methods taking `threshold` and the other three
          # fixed at the 5 they round from.
          if (x_num == 0 && symmetric) {
            for (m in mirrored) {
              t_m <- if (endsWith(m, "_from")) threshold else 5
              half <- unround(
                x_str,
                rounding = m,
                threshold = threshold,
                digits = digits,
                symmetric = TRUE
              )
              expect_equal(
                half$upper - half$lower,
                2 * t_m * unit / 10,
                label = paste(
                  m, "at zero | symmetric | threshold =", threshold,
                  "| digits =", digits
                )
              )
              expect_equal(half$lower, -half$upper)
              expect_equal(half$incl_lower, half$incl_upper)
            }
          }
        }
      }
    }
  }
})


test_that("the `*_from` methods only move a range, never widen it", {
  # At every threshold, each of the three spans exactly one step, just as
  # `"up"`, `"down"`, and `"up_or_down"` do at the 5 they are fixed to. The two
  # constituents span the very same interval and differ only in which endpoint
  # each includes, so their union is that interval with both ends included.
  for (threshold in seq(1, 9)) {
    up <- unround("5.00", rounding = "up_from", threshold = threshold)
    down <- unround("5.00", rounding = "down_from", threshold = threshold)
    both <- unround(
      "5.00",
      rounding = "up_from_or_down_from",
      threshold = threshold
    )
    expect_equal(up$lower, 5 + (threshold - 10) / 1000)
    expect_equal(up$upper, 5 + threshold / 1000)
    expect_equal(c(down$lower, down$upper), c(up$lower, up$upper))
    expect_equal(c(both$lower, both$upper), c(up$lower, up$upper))

    expect_equal(c(up$incl_lower, up$incl_upper), c(TRUE, FALSE))
    expect_equal(c(down$incl_lower, down$incl_upper), c(FALSE, TRUE))
    expect_equal(c(both$incl_lower, both$incl_upper), c(TRUE, TRUE))
  }
})


test_that("a threshold cannot make a consistency test decide nothing", {
  # The practical consequence of the width invariant. With the old bounds,
  # `grim()` under `threshold = 9` passed *every* mean at `n = 57`, because the
  # reconstructed range was wide enough to reach a possible mean whatever the
  # reported value was. Moving the tie point shifts the window without resizing
  # it, so the count of consistent means does not depend on the threshold.
  x <- as.numeric(sprintf("%.2f", seq(0, 9.99, by = 0.01)))
  for (n in c(28, 57)) {
    at_5 <- sum(grim(x, n, digits_x = 2, rounding = "up_or_down"))
    for (threshold in c(1, 3, 7, 9)) {
      expect_equal(
        sum(grim(
          x, n,
          digits_x = 2,
          rounding = "up_from_or_down_from",
          threshold = threshold
        )),
        at_5,
        label = paste("`grim()` at n =", n, "| threshold =", threshold)
      )
    }
  }
})
