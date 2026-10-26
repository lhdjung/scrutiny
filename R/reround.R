# The three compound rounding methods return two values per input value,
# interleaved as `c(up_1, down_1, up_2, down_2, ...)`, so each input's pair
# stays together. `grimmer_scalar()` relies on that to keep its candidates
# apart; pooling them across candidates would let a case pass falsely (#85).

interleave_pair <- function(first, second) {
  out <- rep_len(NA_real_, length(first) + length(second))
  out[c(TRUE, FALSE)] <- first
  out[c(FALSE, TRUE)] <- second
  out
}


# Despite the name, this is vectorized over `x` and `digits`. It is scalar only
# in `rounding`, `threshold`, and `symmetric`, which `reround()` enforces.

reconstruct_rounded_numbers_scalar <- function(
  x,
  digits,
  rounding,
  threshold,
  symmetric
) {
  switch(
    rounding,
    "up_or_down" = interleave_pair(
      round_up(x, digits, symmetric),
      round_down(x, digits, symmetric)
    ),
    "up_from_or_down_from" = {
      check_threshold_valid(threshold)
      interleave_pair(
        round_up_from(x, digits, threshold, symmetric),
        round_down_from(x, digits, threshold, symmetric)
      )
    },
    "ceiling_or_floor" = interleave_pair(
      round_ceiling(x, digits),
      round_floor(x, digits)
    ),
    "even" = round(x, digits),
    "up" = round_up(x, digits, symmetric),
    "down" = round_down(x, digits, symmetric),
    # Only the `"*_from"` methods use `threshold`, so only they validate it:
    "up_from" = {
      check_threshold_valid(threshold)
      round_up_from(x, digits, threshold, symmetric)
    },
    "down_from" = {
      check_threshold_valid(threshold)
      round_down_from(x, digits, threshold, symmetric)
    },
    "ceiling" = round_ceiling(x, digits),
    "floor" = round_floor(x, digits),
    "trunc" = round_trunc(x, digits),
    "anti_trunc" = round_anti_trunc(x, digits),
    abort_in_export(
      "`rounding` must be one of the designated string values.",
      "x" = "It was given as {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    )
  )
}


#' General interface to reconstructing rounded numbers
#'
#' @description `reround()` rounds a vector of reconstructed values the way
#'   they are assumed to have been rounded originally. It is the common
#'   interface to all of scrutiny's rounding functions and to
#'   [`base::round()`], and the function that [`debit()`] uses to round
#'   reconstructed SDs under `formula = "mean_n"`.
#'
#' @details `reround()` calls the rounding function that `rounding` names. See
#'   `vignette("rounding-options")` for the possible values, and [`round_up()`]
#'   and [`round_ceiling()`] for the functions themselves.
#'
#' @param x Numeric. Values to round.
#' @param digits Integer. Number of decimal places to round to, i.e., the
#'   number in the reported values. Length 1 or the length of `x`. Must be
#'   whole numbers; a missing value propagates to a missing result.
#' @param rounding String (length 1). The rounding method. Default is
#'   `"up_or_down"`, which returns two values per element of `x`: rounded up
#'   and rounded down.
#' @param threshold Numeric (length 1). For `"up_from"`, `"down_from"`, and
#'   `"up_from_or_down_from"`, the point within a step at which rounding
#'   switches direction, in tenths of a step; greater than `0` and less than
#'   `10`. Default is `5`, which makes these methods the same as `"up"`,
#'   `"down"`, and `"up_or_down"`. See [`round_up_from()`].
#' @param symmetric Logical (length 1). If `TRUE`, negative numbers are rounded
#'   like their absolute values under `"up"`, `"down"`, `"up_or_down"`, and the
#'   `"*_from"` methods. Default is `FALSE`. Must not be given with a `"ties_*"`
#'   method, which already fixes the direction: `"ties_up"` with `symmetric =
#'   TRUE` would be `"ties_away"`, so write that instead.
#'
#' @include utils.R round.R round-ceil-floor.R
#'
#' @export
#'
#' @return Numeric. One value per element of `x`, except for the compound
#'   methods `"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`,
#'   which return two: the results of their two constituent procedures, next to
#'   each other, as `c(up_1, down_1, up_2, down_2, ...)`. Each pair belongs to
#'   one element of `x`, so the pairs must not be pooled across elements.

reround <- function(
  x,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # The last three arguments describe one rounding procedure for all of `x`.
  # Pairing values with procedures by position would be too confusing:
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # R's own recycling would silently round extra values at the wrong decimal
  # level. No pairing warning: a per-value `digits` is the ordinary way to call
  # this.
  check_lengths_congruent(list(x, digits), warn = FALSE)

  # A fractional `digits` would scale `x` by a non-power of ten and return a
  # number on no decimal grid, whereas `base::round()` rounds `digits` itself:
  check_digits_whole(digits)

  # `rounding_offsets()` resolves `"ties_*"` strings through the same helper:
  spec <- resolve_ties_rounding(rounding, symmetric)

  # Adding zero turns a negative zero, as from `ceiling(-0.5)`, into zero, which
  # `sprintf()` would otherwise print as `"-0.00"`:
  out <- reconstruct_rounded_numbers_scalar(
    x,
    digits,
    spec$rounding,
    threshold,
    spec$symmetric
  )
  `attributes<-`(out + 0, NULL)
}
