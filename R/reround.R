# # Full example inputs:
# x <- 65.3488492
# digits <- 2
# rounding <- "up_or_down"
# threshold <- 5
# symmetric <- FALSE

# The three compound rounding methods return two values per input value, and
# they return them interleaved: `c(up_1, down_1, up_2, down_2, ...)`, so that
# each input value's own pair of results stays together. `grimmer_scalar()`
# relies on this layout to keep the candidates apart -- pooling them across
# candidates was the false-pass bug #85.
#
# Interleaving is what `Vectorize()` produced anyway, one column of the result
# matrix per input value; doing it explicitly is what lets
# `reconstruct_rounded_numbers_scalar()` take a whole vector at once (see
# `reround()` below).

interleave_pair <- function(first, second) {
  out <- rep_len(NA_real_, length(first) + length(second))
  out[c(TRUE, FALSE)] <- first
  out[c(FALSE, TRUE)] <- second
  out
}


# Despite the name, this is vectorized over `x` and `digits` -- every rounding
# function it dispatches to is. It is scalar in `rounding`, `threshold`, and
# `symmetric`, which is why `reround()` still vectorizes it for the rare call
# that varies those.

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


reconstruct_rounded_numbers <- Vectorize(
  reconstruct_rounded_numbers_scalar,
  USE.NAMES = FALSE
)


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
#'   [`grimmer()`]).
#' @param rounding String. The rounding method that is supposed to have been
#'   used originally. See `vignette("rounding-options")`. Default is
#'   `"up_or_down"`, which returns two values: `x` rounded up *and* down.
#' @param threshold Numeric. If `rounding` is set to `"up_from"`, `"down_from"`,
#'   or `"up_from_or_down_from"`, `threshold` is the point within a step at
#'   which rounding switches direction, in tenths of a step; it must be greater
#'   than `0` and less than `10`. Otherwise irrelevant. Default is `5`, which
#'   makes those three methods the same as `"up"`, `"down"`, and `"up_or_down"`.
#'   See [`round_up_from()`], which spells out how `round_down_from()` mirrors
#'   the threshold.
#' @param symmetric Logical. Set `symmetric` to `TRUE` if the rounding of
#'   negative numbers with `"up_or_down"`, `"up"`, `"down"`,
#'   `"up_from_or_down_from"`, `"up_from"`, or `"down_from"` should mirror that
#'   of positive numbers so that their absolute values are always equal.
#'   Otherwise irrelevant. Default is `FALSE`. It only ever affects ties in
#'   negative numbers, but `TRUE` is what reconstructs Excel, SAS, SPSS, and
#'   Matlab; see `vignette("rounding-options")`.
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
  # The overwhelmingly common case is a single rounding procedure applied to a
  # whole vector of values -- that is what every consistency test does, once per
  # candidate value per row. Every `round_*()` function is natively vectorized,
  # so only the dispatch below has to be scalar, and it can be done once instead
  # of once per element:
  if (
    length(rounding) == 1L &&
      length(threshold) == 1L &&
      length(symmetric) == 1L
  ) {
    return(`attributes<-`(
      reconstruct_rounded_numbers_scalar(
        x,
        digits,
        rounding,
        threshold,
        symmetric
      ),
      NULL
    ))
  }

  # For calls with multiple rounding procedures, each individual procedure needs
  # to be singular; i.e., `rounding` can either be (1) a string vector of length
  # 1 indicating two procedures, such as `"up_or_down"`; or (2) a string vector
  # of any length with values such as `"up"` or `"even"`, but not
  # `"up_or_down"`:
  if (length(rounding) > 1L) {
    check_rounding_singular(rounding, "up_or_down", "up", "down")
    check_rounding_singular(
      rounding,
      "up_from_or_down_from",
      "up_from",
      "down_from"
    )
    check_rounding_singular(rounding, "ceiling_or_floor", "ceiling", "floor")
    # Throw an error if the lengths of the first two arguments are inconsistent:
    if (length(x) > 1L && length(x) != length(rounding)) {
      cli::cli_abort(c(
        "!" = "`x` and `rounding` must have the same length \\
      unless either has length 1.",
        "i" = "`x` has length {length(x)}.",
        "i" = "`rounding` has length {length(rounding)}."
      ))
    }
  }

  # Go through the rounding options and, once the correct option (as per
  # `rounding`) has been found, proceed as described in the `Details` section of
  # the documentation. To vectorize the arguments, this is done via the helper
  # function at the top of the present file. Finally, attributes are removed.
  # This is because the helper returns a matrix structure.
  `attributes<-`(
    reconstruct_rounded_numbers(x, digits, rounding, threshold, symmetric),
    NULL
  )
}
