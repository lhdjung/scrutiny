# Package-level constants. They live here, and in SCREAMING_SNAKE_CASE, so that
# a name in any other file is recognizable as a constant on sight rather than as
# a local variable that happens to be in scope. The file is named `aaa.R`
# because roxygen2 sorts unconstrained files alphabetically when it writes the
# `Collate` field, which puts this one first.
#
# All of them are evaluated when the package is installed, not when it is
# loaded, so the values are fixed at build time. For the two tolerances below
# that is deliberate: they are derived from `.Machine$double.eps`, which is
# `2.220446e-16` on every platform R runs on -- R requires IEEE 754 doubles --
# so nothing is lost by freezing it, and a consistency test that gives one
# verdict on the author's machine and another on a reader's would contradict the
# purpose of the package. If a run-machine value were ever wanted instead, it
# would take an `.onLoad()` hook that recomputes them; there is none, and there
# should not be one without a reason.

# The tolerance below which a difference from the nearest whole number counts as
# none, used by `is_whole_number()` and by the check that `digits_x` and
# `digits_sd` are whole. It is ten times `ROUNDING_TOLERANCE`, which is a nudge
# applied to a scaled value rather than a margin around an integer.
#
# It is a constant rather than a default argument spelled
# `.Machine$double.eps^0.5` in place, as it was until scrutiny 1.0.0: that
# default was re-evaluated on every call -- a lookup in the `.Machine` list plus
# a square root -- which cost more than the arithmetic it is a tolerance for,
# once per key value per row (#92).

WHOLE_NUMBER_TOLERANCE <- .Machine$double.eps^0.5


# Shifting a number by `digits` decimal places is not exact in floating point:
# `0.28 * 100` is 28.000000000000004, and `0.29 * 100` is 28.999999999999996.
# Rounding the shifted value away from the number it is meant to be would then
# move it a whole step -- `ceiling(0.28 * 100) / 100` would be 0.29 rather than
# 0.28. Every rounding function in round.R and round-ceil-floor.R therefore
# nudges the shifted value by this tolerance before rounding it: the `round_*()`
# functions of round-ceil-floor.R add or subtract it directly, and
# `round_up_from()` and `round_down_from()` fold it into `tie_offset_up()` and
# `tie_offset_down()`, respectively. It is
# far smaller than any difference a reported value could meaningfully express,
# so it only ever absorbs representation error.
#
# `unround()` reports bounds that assume exactly this tolerance, and the
# property test in test-unround.R checks that the two agree, so all three files
# have to stay with the one constant.
#
# The tolerance is absolute, so it has a domain of validity: representation
# error in `x * 10^digits` grows with the magnitude of that product (roughly
# `|x| * 10^digits * 2.2e-16`), whereas the nudge is fixed. Up to about `|x *
# 10^digits| = 1e7` the nudge dominates by orders of magnitude; far beyond that,
# a value sitting exactly on a rounding boundary may go either way. Means, SDs,
# and percentages with a few decimal places are nowhere near that.

ROUNDING_TOLERANCE <- WHOLE_NUMBER_TOLERANCE / 10


# The `"ties_*"` rounding strings each name a complete tie-breaking procedure,
# so one of them says by itself what `rounding` plus `symmetric` says together.
# `reround()` and `rounding_offsets()` both resolve them through this one table,
# so the forward functions and the bounds can't come to disagree about what a
# name means.
#
# `symmetric` is not consulted for them, and giving it is an error rather than a
# no-op: the procedure is already fully determined by the name, so a
# `"ties_away"` that a separate argument could turn into something else would
# defeat the point of naming it -- but silently dropping the argument threw away
# half of what the caller wrote, and did so in the one case where they were most
# likely to mean it. `resolve_ties_rounding()` (`R/utils.R`) is where that is
# decided, for the forward functions and the bounds alike.
#
# `"ties_even"` resolves to `"even"`, which is `base::round()`, and carries a
# `symmetric` of `FALSE` only because the field has to say something: rounding
# to even is its own mirror image, so the value is never read for it. It is here
# because *roundTiesToEven* is IEEE 754's default direction and the one R,
# Python, and NumPy take, which made it the conspicuous absence from a family
# whose whole point is naming the tie rule. With it, the five names in this
# table plus `"ceiling"`, `"floor"`, and `"trunc"` cover every rounding
# direction the standard defines.

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
