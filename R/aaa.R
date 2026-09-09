# Package-level constants, in SCREAMING_SNAKE_CASE so that a name elsewhere is
# recognizable as one on sight. The file is named `aaa.R` because roxygen2 sorts
# unconstrained files alphabetically into `Collate`, which puts this one first.
#
# They are evaluated at install time, not load time. For the two tolerances that
# is deliberate: `.Machine$double.eps` is `2.220446e-16` on every platform R
# runs on, so nothing is lost by freezing it, and a verdict must not depend on
# the machine that reached it. A run-machine value would take an `.onLoad()`
# hook; there is none, and there should not be one without a reason.

# The tolerance below which a difference from the nearest whole number counts as
# none, used by `is_whole_number()` and by the check that `digits_x` and
# `digits_sd` are whole. Ten times `ROUNDING_TOLERANCE`, which is a nudge on a
# scaled value rather than a margin around an integer.
#
# A constant rather than a default argument spelled `.Machine$double.eps^0.5`
# in place: that was re-evaluated on every call -- a list lookup plus a square
# root -- costing more than the arithmetic it is a tolerance for (#92).

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
# `unround()`'s bounds assume exactly this tolerance, and the property test in
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
# `symmetric` is not consulted for them, and giving it is an error rather than a
# no-op -- see `resolve_ties_rounding()` (`R/utils.R`), which decides that for
# both sides.
#
# `"ties_even"` resolves to `base::round()` and carries `symmetric = FALSE` only
# because the field has to say something: rounding to even is its own mirror
# image. It is here because *roundTiesToEven* is IEEE 754's default and R's, so
# it was the conspicuous absence from a family that names tie rules. With it,
# these five names plus `"ceiling"`, `"floor"`, and `"trunc"` cover every
# rounding direction the standard defines.

# fmt: skip
TIES_METHODS <- list(
  ties_up   = list(rounding = "up",   symmetric = FALSE),  # toward +Inf
  ties_down = list(rounding = "down", symmetric = FALSE),  # toward -Inf
  ties_away = list(rounding = "up",   symmetric = TRUE),   # roundTiesToAway
  ties_zero = list(rounding = "down", symmetric = TRUE),   # toward zero
  ties_even = list(rounding = "even", symmetric = FALSE)   # roundTiesToEven
)


# The user-facing consistency test functions: `grim()`, `grim_map()`,
# `grimmer_map_seq()`, `debit_map_total_n()`, and so on.

PATTERN_NAME_TEST_FN <- "^(grim|grimmer|debit)"

# Used in `grim_plot()` if `digits > 2`
TIBBLE_FRAC_N_ZERO <- tibble::new_tibble(list(frac = 0, n = 0L), nrow = 1L)


# List of minimal-distance functions for `audit_seq()`
#
# The functions collected in `LIST_MIN_DISTANCE_FUNCTIONS` are mapped in one
# particular place within `audit_seq()` and shouldn't really be used elsewhere.
#
# Instead of being individually defined as named functions or being used as
# anonymous functions directly inside of `audit_seq()`, they are stored in a
# list for greater efficiency -- in terms of both speed and memory.
#
# The `x` parameter in all three functions is an integer vector measuring the
# number of dispersion steps between inconsistent reported values and their
# consistent neighbors. The notion of "steps" is the same as in, e.g.,
# `grim_map_seq()`.
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
