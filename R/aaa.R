# Package-level constants, in SCREAMING_SNAKE_CASE so that a name elsewhere is
# recognizable as one on sight. The file is named `aaa.R` because roxygen2 sorts
# unconstrained files alphabetically into `Collate`, which puts this one first.
#
# They are evaluated at install time, not load time. For the two tolerances that
# is deliberate: `.Machine$double.eps` is `2.220446e-16` on every platform R
# runs on, and a verdict must not depend on the machine that reached it.

# The tolerance below which a difference from the nearest whole number counts as
# none, used by `is_whole_number()` and by the check that `digits_x` and
# `digits_sd` are whole. Ten times `ROUNDING_TOLERANCE`, which is a nudge on a
# scaled value rather than a margin around an integer.
#
# A constant rather than a default argument `.Machine$double.eps^0.5`, which
# would be evaluated on every call, costing more than the arithmetic it is a
# tolerance for.

WHOLE_NUMBER_TOLERANCE <- .Machine$double.eps^0.5


# Shifting a number by `digits` decimal places is not exact in floating point:
# `0.28 * 100` is 28.000000000000004. Rounding the shifted value away from the
# number it is meant to be would move it a whole step, making
# `ceiling(0.28 * 100) / 100` 0.29. Every rounding function therefore nudges the
# shifted value by this tolerance first -- directly in round-ceil-floor.R, and
# folded into `tie_offset_up()` / `tie_offset_down()` in round.R. It is far
# smaller than any difference a reported value could express, so it only absorbs
# representation error.
#
# `unround()`'s bounds assume exactly this tolerance, and the sweep in
# test-unround.R checks that the two agree: change one and change the other.
#
# The tolerance is absolute, so it has a domain of validity: representation
# error in `x * 10^digits` grows with that product (roughly
# `|x| * 10^digits * 2.2e-16`) while the nudge is fixed. Up to about
# `|x * 10^digits| = 1e7` the nudge dominates by orders of magnitude; far beyond
# it, a value exactly on a rounding boundary may go either way. Reported means,
# SDs, and percentages are nowhere near that.

ROUNDING_TOLERANCE <- WHOLE_NUMBER_TOLERANCE / 10


# Each `"ties_*"` string names a complete tie-breaking procedure, so it says by
# itself what `rounding` plus `symmetric` say together. `reround()` and
# `rounding_offsets()` both resolve them through this one table, so the forward
# functions and the bounds cannot disagree about what a name means.
#
# Giving `symmetric` alongside one of them is an error; see
# `resolve_ties_rounding()` in `R/utils.R`.
#
# `"ties_even"` resolves to `base::round()`. Its `symmetric = FALSE` is a
# placeholder: rounding to even is its own mirror image. These five names plus
# `"ceiling"`, `"floor"`, and `"trunc"` cover every rounding direction that
# IEEE 754 defines.

# fmt: skip
TIES_METHODS <- list(
  ties_up   = list(rounding = "up",   symmetric = FALSE),  # toward +Inf
  ties_down = list(rounding = "down", symmetric = FALSE),  # toward -Inf
  ties_away = list(rounding = "up",   symmetric = TRUE),   # roundTiesToAway
  ties_zero = list(rounding = "down", symmetric = TRUE),   # toward zero
  ties_even = list(rounding = "even", symmetric = FALSE)   # roundTiesToEven
)


# The values of `sd_meaning` in GRIMMER and DEBIT: what a value reported as an
# SD really is. The sample SD divides by `n - 1`, the population SD by `n`,
# and `"sd_sample_or_pop"` accepts a value set that passes under one or
# the other. `"se"` is the standard error of the mean, the sample SD divided by
# `sqrt(n)`, for an SE reported as if it were an SD. Each value is one premise,
# so a new one takes a row in `sd_denominator()`. And the values of DEBIT's
# `formula`.
#
# Every function with one of these arguments spells the values out as its
# default, so that they show in its usage. The `*_scalar()` functions check
# against the constants, which is cheaper than reading their own formals once
# per row. A test in test-utils.R requires every default to equal its constant.
SD_MEANINGS <- c("sd_sample", "sd_pop", "sd_sample_or_pop", "se")
DEBIT_FORMULAS <- c("exact", "mean_n")


# The strings that `as.numeric()` reads as an infinity or `NaN`, once their
# letters are lowercased. `decimal_places()` and `decimal_places_scalar()` give
# them `NA` decimal places, via `is_non_finite_token()`.
NON_FINITE_TOKENS <- c(outer(
  c("", "+", "-"),
  c("inf", "infinity", "nan"),
  paste0
))


# Placeholder data for `grim_plot_grid()`'s tile layer when there is no
# raster, as with `digits > 2`:
TIBBLE_FRAC_N_ZERO <- tibble::new_tibble(list(frac = 0, n = 0L), nrow = 1L)


# The `diff_*` distances in `audit_seq()`, passed to `dplyr::across()` as one
# list. `x` is the number of dispersion steps between a reported value and its
# consistent neighbors, as in `grim_map_seq()`.
LIST_MIN_DISTANCE_FUNCTIONS <- list(
  # Absolute distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          min(abs(x), na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Positive distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          min(x[x > 0L], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  },

  # Negative distance:
  function(x) {
    vapply(
      x,
      function(x) {
        if (all(is.numeric(x))) {
          max(x[x < 0L], na.rm = TRUE)
        } else {
          NA_real_
        }
      },
      numeric(1L),
      USE.NAMES = FALSE
    )
  }
)
