# An independent check on GRIM's arithmetic.
#
# `grim()`, `grim_map(show_rec = TRUE)`, `grim_values()`, and `grim_closest()`
# all read off the same sum range from `sum_range()`, which derives it in closed
# form and in exact integer arithmetic. Instead, the oracle below derives
# nothing, but rather takes candidate sum totals one at a time, divides each by
# `n * items`, rounds the quotient the way the reported `x` was presumably
# rounded, and checks whether the result is `x`. That is GRIM's definition,
# applied systematically in a grid sweep, and it goes through `reround()` -- the
# actual rounding functions -- rather than through the offsets table that the
# tests derive their bounds from.
#
# `test-grim.R` has a narrower oracle of the same kind, covering the verdict of
# `grim()` alone under `"up_or_down"`, `"up"`, and `"down"`. This one widens the
# parameter space -- every rounding method, both boundary thresholds,
# `symmetric`, `items`, `percent`, and negative means -- and checks not just the
# verdict but the numbers the verdict is made of: `sum_lower` and `sum_upper`,
# the granules, and the values that `grim_values()` and `grim_closest()` derive
# from the same range. One method is left out of the equality check on purpose:
#
# - `rounding = "even"`, whose bounds are deliberately treated as inclusive
#   although `base::round()` may not include them. The tests are then knowingly
#   too permissive, so the oracle would report a disagreement that is a design
#   decision, not an error. See the comment on `rounding_offsets()`. It is not
#   left untested, though: the last test below checks the half of the property
#   that must hold for it, which is that it is never too *strict*.
#
# Each combination of parameters is tested with a single call per function and a
# handful of expectations on whole columns, rather than one call and a dozen
# expectations per value set. The oracle is the slow part -- it walks the range
# sum by sum -- so it, too, rounds a whole row's worth of candidates at once.

# Which of `sums` would have been reported as `x_num`? The "_or_" rounding
# methods return one value per rounding variant, blocked by input value, and
# either variant hitting `x_num` makes the sum total admissible:
sums_round_back <- function(
  sums,
  x_num,
  n_items,
  digits,
  rounding,
  threshold,
  symmetric
) {
  granules <- suppressWarnings(reround(
    sums / n_items,
    digits = digits,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  ))
  hits <- abs(granules - x_num) < 1e-9
  hits[is.na(hits)] <- FALSE
  colSums(matrix(hits, ncol = length(sums))) > 0L
}


# The admissible sums are the whole numbers in an interval, hence contiguous, so
# the range `[lower, upper]` is exactly right if every sum inside it
# reconstructs `x_num` (the range is not too wide) and neither of the two sums
# just outside it does (it is not too narrow). For an empty range -- `lower` is
# then `upper + 1` -- the two straddling sums are the only ones to check, and
# the pattern below is `FALSE`, `FALSE`.
oracle_verdicts <- function(
  lower,
  upper,
  x_num,
  n_items,
  digits,
  rounding,
  threshold,
  symmetric
) {
  sums <- seq(lower - 1L, upper + 1L)
  admissible <- sums_round_back(
    sums,
    x_num,
    n_items,
    digits,
    rounding,
    threshold,
    symmetric
  )
  expected <- c(FALSE, rep(TRUE, max(0L, upper - lower + 1L)), FALSE)
  identical(admissible, expected)
}


# `threshold` is only meaningful for the three `"*_from"` methods. Every other
# method ignores it, so it is not varied for them:
grim_oracle_grid <- function() {
  # fmt: skip
  methods_fixed <- c(
    "up_or_down", "up", "down", "ceiling", "floor", "ceiling_or_floor",
    "trunc", "anti_trunc", "ties_up", "ties_down", "ties_away", "ties_zero"
  )
  methods_threshold <- c("up_from", "down_from", "up_from_or_down_from")
  rbind(
    expand.grid(
      rounding = methods_fixed,
      threshold = 5,
      stringsAsFactors = FALSE
    ),
    expand.grid(
      rounding = methods_threshold,
      threshold = c(3, 7),
      stringsAsFactors = FALSE
    )
  )
}


test_grim_oracle <- function(x, n, digits_x, items, percent, symmetric) {
  grid <- grim_oracle_grid()
  data <- tibble::tibble(x = x, n = n)

  # GRIM works with decimal numbers internally, so a percentage is divided by
  # 100 and given two more decimal places. The sums are counts of the underlying
  # data either way, but the granules and the achievable means come back on the
  # scale of `x`:
  scale_x <- if (percent) 100 else 1
  x_num <- x / scale_x
  digits_num <- if (percent) digits_x + 2L else digits_x
  n_items <- n * items

  for (i in seq_len(nrow(grid))) {
    rounding <- grid$rounding[i]
    threshold <- grid$threshold[i]

    # fmt: skip
    info <- paste0(
      "digits_x = ", digits_x, ", items = ", items, ", percent = ", percent,
      ", rounding = ", rounding, ", threshold = ", threshold,
      ", symmetric = ", symmetric
    )

    out <- grim_map(
      data,
      digits_x = digits_x,
      items = items,
      percent = percent,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric,
      show_rec = TRUE
    ) |>
      suppressMessages()

    values <- grim_values(
      x,
      n,
      digits_x = digits_x,
      items = items,
      percent = percent,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )
    closest <- grim_closest(
      x,
      n,
      digits_x = digits_x,
      items = items,
      percent = percent,
      rounding = rounding,
      threshold = threshold,
      symmetric = symmetric
    )

    lower <- out$sum_lower
    upper <- out$sum_upper
    consistency <- lower <= upper

    # THE ORACLE: the displayed range is exactly the range of admissible sums.
    agrees <- vapply(
      seq_len(nrow(out)),
      function(j) {
        oracle_verdicts(
          lower = lower[j],
          upper = upper[j],
          x_num = x_num[j],
          n_items = n_items[j],
          digits = digits_num,
          rounding = rounding,
          threshold = threshold,
          symmetric = symmetric
        )
      },
      logical(1L)
    )
    expect_true(
      all(agrees),
      info = paste0(
        info,
        " -- rows: ",
        toString(
          which(!agrees)
        )
      )
    )

    # The verdict is that range being non-empty, and nothing else:
    expect_equal(out$consistency, consistency, info = info)

    # An empty range is empty by exactly one: `sum_lower` and `sum_upper` are
    # then the whole numbers straddling `rec_sum`. This is what keeps the gap
    # from meaning anything beyond inconsistency itself:
    expect_true(
      all(lower[!consistency] - upper[!consistency] == 1),
      info = info
    )

    # `rec_sum` is a sum of the underlying data, so `percent` leaves it alone;
    # the granules are read against `x`, so they follow its scale:
    expect_equal(out$rec_sum, x_num * n_items, info = info)
    expect_equal(
      out$rec_x_lower,
      floor(x_num * n_items + 1e-9) * scale_x / n_items,
      info = info
    )
    expect_equal(
      out$rec_x_upper,
      ceiling(x_num * n_items - 1e-9) * scale_x / n_items,
      info = info
    )

    # Every achievable mean is one of the admissible sums divided by `n *
    # items`, and there are none at all if the value set is inconsistent:
    achievable <- lapply(seq_len(nrow(out)), function(j) {
      if (consistency[j]) {
        seq(lower[j], upper[j]) * scale_x / n_items[j]
      } else {
        numeric(0L)
      }
    })
    expect_equal(values, achievable, info = info)

    # `grim_closest()` is one of the achievable means -- or, for an inconsistent
    # value set, one of the two straddling it -- and no other one is closer to
    # `x`:
    reachable <- lapply(seq_len(nrow(out)), function(j) {
      if (consistency[j]) {
        achievable[[j]]
      } else {
        c(upper[j], lower[j]) * scale_x / n_items[j]
      }
    })
    is_reachable <- purrr::map2_lgl(
      reachable,
      closest,
      function(values_j, closest_j) any(abs(values_j - closest_j) < 1e-9)
    )
    expect_true(all(is_reachable), info = info)
    expect_equal(
      abs(closest - x),
      vapply(
        seq_along(reachable),
        function(j) min(abs(reachable[[j]] - x[j])),
        numeric(1L)
      ),
      info = info
    )
  }
}


# `-0.1` and `0.1` are the means whose rounding bounds reach exactly zero at one
# decimal place, and `0` is where `"trunc"` and `"anti_trunc"` part company most
# sharply -- the widest range of any method for the first, a single point for
# the second:
x_oracle <- c(-2.5, -0.71, -0.1, 0, 0.1, 0.24, 1.99, 4.2, 5.19)

# A bound lands exactly on a whole-number sum total -- where its inclusivity is
# what decides the case -- only for `n * items` that share enough factors of ten
# with `10^(digits_x + 1)`. At one decimal place, the offsets of 5 need a
# multiple of 20, those of 10 a multiple of 10, and the `threshold` offsets of 3
# and 7 a multiple of 100. The other sample sizes are there for the ordinary
# case, where no bound is attainable:
n_oracle <- c(3L, 12L, 20L, 40L, 100L, 200L)

data_oracle <- tidyr::expand_grid(x = x_oracle, n = n_oracle)


test_that("GRIM agrees with the oracle for every rounding method", {
  for (digits_x in 1:2) {
    for (symmetric in c(FALSE, TRUE)) {
      test_grim_oracle(
        x = round(data_oracle$x, digits_x),
        n = data_oracle$n,
        digits_x = digits_x,
        items = 1,
        percent = FALSE,
        symmetric = symmetric
      )
    }
  }
})


test_that("GRIM agrees with the oracle for multi-item scales", {
  test_grim_oracle(
    x = data_oracle$x,
    n = data_oracle$n,
    digits_x = 2,
    items = 3,
    percent = FALSE,
    symmetric = FALSE
  )
})


test_that("GRIM agrees with the oracle for percentages", {
  # A percentage is a whole number often enough to be worth testing at zero
  # decimal places, which is where `x` and its granules are furthest apart:
  data_percent <- tidyr::expand_grid(x = c(0, 24, 71, 84, 100), n = n_oracle)
  for (digits_x in 0:1) {
    test_grim_oracle(
      x = data_percent$x,
      n = data_percent$n,
      digits_x = digits_x,
      items = 1,
      percent = TRUE,
      symmetric = FALSE
    )
  }
})


# `"even"` is the one method whose bounds cannot be pinned down, because
# `base::round()` breaks a tie by the parity of the binary double rather than by
# a rule on the decimal value. `rounding_offsets()` therefore reports both of
# its bounds as inclusive, which can only ever widen the range. That makes the
# equality above inapplicable, but it leaves the half that must still hold: a
# value set that `base::round()` really can produce must never be called
# inconsistent. Being too permissive is the safe direction for an
# error-detection tool; being too strict would be a false accusation.

test_that("`\"even\"` is never too strict, only ever too permissive", {
  # Its own grid: the disagreements live where a granule falls exactly on a
  # rounding boundary, which for `base::round()` means a sample size of 4, 8 or
  # 40 and a mean whose last digit makes `x * n` land on a half. The grid of the
  # tests above contains no such case, so reusing it would leave the
  # over-permissive direction unexercised.
  n_permissive <- 0L
  n_reachable <- 0L

  for (digits_x in 1:2) {
    unit <- 10^-digits_x
    grid <- tidyr::expand_grid(
      x = round(seq(-30, 30) * unit, digits_x),
      n = c(4L, 8L, 20L, 40L)
    )
    verdict <- grim(grid$x, grid$n, digits_x = digits_x, rounding = "even")

    for (i in seq_len(nrow(grid))) {
      # Every candidate sum total around `x * n`, asked of `base::round()`
      # itself through `reround()` rather than of the offsets table:
      sums <- seq(floor(grid$x[i] * grid$n[i]) - 3L, ceiling(grid$x[i] * grid$n[i]) + 3L)
      reachable <- any(sums_round_back(
        sums,
        x_num = grid$x[i],
        n_items = grid$n[i],
        digits = digits_x,
        rounding = "even",
        threshold = 5,
        symmetric = FALSE
      ))

      if (reachable) {
        n_reachable <- n_reachable + 1L
        # The direction that matters: no false negatives, ever.
        expect_true(
          isTRUE(verdict[[i]]),
          label = paste0(
            "x = ", grid$x[i], ", n = ", grid$n[i], ", digits_x = ", digits_x,
            " is reachable but GRIM says ", verdict[[i]]
          )
        )
      } else if (isTRUE(verdict[[i]])) {
        n_permissive <- n_permissive + 1L
      }
    }
  }

  # Guard against a vacuous pass: the sweep has to contain reachable cases for
  # the expectation above to have meant anything.
  expect_gt(n_reachable, 50L)

  # And the documented over-permissiveness is real, not hypothetical. If this
  # ever drops to zero, `"even"` has become exact and the carve-out above can
  # go.
  expect_gt(n_permissive, 0L)
})
