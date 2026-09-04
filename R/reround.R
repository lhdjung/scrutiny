# The three compound rounding methods return two values per input value,
# interleaved as `c(up_1, down_1, up_2, down_2, ...)`, so each input's pair
# stays together. `grimmer_scalar()` relies on that to keep its candidates
# apart -- pooling them across candidates was the false-pass bug #85.

interleave_pair <- function(first, second) {
  out <- rep_len(NA_real_, length(first) + length(second))
  out[c(TRUE, FALSE)] <- first
  out[c(FALSE, TRUE)] <- second
  out
}


# Despite the name, this is vectorized over `x` and `digits` -- every rounding
# function it dispatches to is. It is scalar in `rounding`, `threshold`, and
# `symmetric`, which `reround()` enforces before calling it. The name is a
# leftover from when `reround()` wrapped it in `Vectorize()`.

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
    # The `"*_from"` methods are the ones that `threshold` applies to, so they
    # are the ones that validate it:
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
    cli::cli_abort(c(
      "`rounding` must be one of the designated string values.",
      "x" = "It was given as {wrong_spec_string(rounding)}.",
      "i" = "See `vignette(\"rounding-options\")`."
    ))
  )
}


#' General interface to reconstructing rounded numbers
#'
#' @description `reround()` takes one or more intermediate reconstructed values
#'   and rounds them in some specific way -- namely, the way they are supposed
#'   to have been rounded originally, in the process that generated the reported
#'   values.
#'
#'   This function provides an interface to all of scrutiny's rounding functions
#'   as well as [`base::round()`]. It is used as a helper within [`grim()`],
#'   [`grimmer()`], and [`debit()`]; and it might find use in other places for
#'   consistency testing or reconstruction of statistical analyses.
#'

#' @details `reround()` internally calls the appropriate rounding function(s)
#'   determined by the `rounding` argument. See `vignette("rounding-options")`
#'   for a complete list of values that `rounding` can take.
#'
#'   For the specific rounding functions themselves, see documentation at
#'   [`round_up()`], [`round_ceiling()`], and [`base::round()`].
#'
#' @param x Numeric. Vector of possibly original values.
#' @param digits Integer. Number of decimal places in the reported key values
#'   (i.e., mean or percentage within [`grim()`], or standard deviation within
#'   [`grimmer()`]). It is the one argument that is vectorized along with `x`,
#'   so it must either have the same length as `x` or length 1, and every value
#'   must be a whole number: a fractional one is not a decimal level at all,
#'   and would scale `x` by a non-power of ten. A missing value propagates to a
#'   missing result, as it does in `x`.
#' @param rounding String. The rounding method that is supposed to have been
#'   used originally. See `vignette("rounding-options")`. Default is
#'   `"up_or_down"`, which returns two values: `x` rounded up *and* down.
#' @param threshold Numeric. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, `threshold` is the point within a step at
#'   which rounding switches direction, in tenths of a step; it must be greater
#'   than `0` and less than `10`. Otherwise irrelevant. Default is `5`, which
#'   makes those three methods the same as `"up"`, `"down"`, and `"up_or_down"`.
#'   `threshold` means the same thing in both directions, so it moves a range
#'   without widening it; see [`round_up_from()`].
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up_or_down"`, `"up"`, `"down"`,
#'   `"up_from_or_down_from"`, `"up_from"`, or `"down_from"` should mirror that
#'   of positive numbers so that their absolute values are always equal.
#'   Otherwise irrelevant. Default is `FALSE`. It only ever affects negative
#'   numbers, and with `"up_or_down"`, `"up"`, and `"down"` only ties among
#'   those; `TRUE` is what reconstructs Excel, SAS, SPSS, and Matlab.
#'
#'   It must not be given with any of the `"ties_*"` methods. Each of those
#'   names a complete tie-breaking procedure, so it already says which way ties
#'   go for negative numbers, and a second argument saying otherwise is an
#'   error rather than a silent no-op: `"ties_up"` with `symmetric = TRUE` is
#'   `"ties_away"`, and `"ties_down"` with it is `"ties_zero"`. See
#'   `vignette("rounding-options")`.
#'
#' @include utils.R round.R round-ceil-floor.R
#'
#' @export
#'
#' @return Numeric. One value per element of `x` -- except for the three
#'   compound methods `"up_or_down"`, `"up_from_or_down_from"`, and
#'   `"ceiling_or_floor"`, which return *two* values per element of `x`: the
#'   result of each of their two constituent procedures.
#'
#'   The two values of a compound method stay next to each other, so the return
#'   value is `c(up_1, down_1, up_2, down_2, ...)` and has length `2 *
#'   length(x)`. Take care not to pool the pairs across elements of `x`: matches
#'   found in different pairs did not come from the same original value. (This
#'   was the cause of a false-pass bug in `grimmer()`; see
#'   <https://github.com/lhdjung/scrutiny/issues/85>.)

reround <- function(
  x,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # The last three arguments describe one rounding procedure; `x` is the vector.
  # Vectors of procedures would mean dispatching per element of `x`, and pairing
  # values with procedures by position is confusing enough that `unround()`,
  # which keeps that behavior for display, warns about it in its docs:
  check_rounding_spec_singular(rounding, threshold, symmetric)

  # `digits` really is vectorized along with `x`: one decimal count per value.
  # R's own recycling passed a short `digits` without a word and rounded the
  # extra values at the wrong decimal level. No pairing warning here -- unlike a
  # rounding method, a per-value `digits` is the ordinary way to call this.
  check_lengths_congruent(list(x, digits), warn = FALSE)

  # A fractional `digits` is not a decimal level: it scales `x` by a non-power of
  # ten (`10^1.5` is about 31.6) and returns a number on no decimal grid, so
  # `reround(1.25, digits = 1.5, rounding = "up")` was 1.264911 while `"even"`
  # gave the whole-number answer -- `base::round()` rounds `digits` itself.
  check_digits_whole(digits)

  # A `"ties_*"` string stands in for a `rounding` and a `symmetric` together.
  # `rounding_offsets()` resolves it through the same helper:
  spec <- resolve_ties_rounding(rounding, symmetric)

  # Every `round_*()` function is natively vectorized, so only the dispatch is
  # scalar, and it happens once for the whole of `x`. Attributes are dropped so
  # that the return value is a bare numeric vector whatever `x` carried:
  `attributes<-`(
    reconstruct_rounded_numbers_scalar(
      x,
      digits,
      spec$rounding,
      threshold,
      spec$symmetric
    ),
    NULL
  )
}
