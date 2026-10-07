# Shared by `reround_to_fraction()` and `reround_to_fraction_level()`. The
# `"auto"` option for `digits` is the same as in `janitor::round_to_fraction()`.
# It has to be resolved before the whole-number check, which is numeric:
# `is.infinite("auto")` is `FALSE`, so the string went straight into
# `is_whole_number()` and failed there with "non-numeric argument to
# mathematical function".
resolve_digits_fraction <- function(digits, denominator) {
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1L
  }

  # A missing `digits` passes, and propagates to a missing result, as it does in
  # `reround()`. So does an infinite one, which `reround_to_fraction()` treats
  # as "don't round":
  digits_finite <- digits[is.finite(digits)]
  if (!all(is_whole_number(digits_finite))) {
    abort_in_export(
      "!" = "Each `digits` value must be a whole number.",
      "x" = "`digits` was given as \\
        {digits_finite[!is_whole_number(digits_finite)]}."
    )
  }

  digits
}


#' Rounding to the nearest fraction
#'
#' @description These functions round to the nearest fraction of a
#'   denominator rather than to a decimal place. They follow
#'   [`janitor::round_to_fraction()`] but take all of [`reround()`]'s options:
#'
#'   - `reround_to_fraction()` rounds to fractions of a whole number, then
#'   optionally rounds the result to `digits` decimal places.
#'   - `reround_to_fraction_level()` rounds to the nearest fraction at the
#'   decimal level given by `digits`, with no further rounding.
#'
#' @param x Numeric. Values to round.
#' @param denominator Numeric (at least 1). `x` is rounded to the nearest
#'   fraction of `denominator`. Default is `1`.
#' @param digits Integer. In `reround_to_fraction()`, the number of decimal
#'   places to round the result to. `"auto"` means
#'   `ceiling(log10(denominator)) + 1`, as in janitor, and the default `Inf`
#'   means no further rounding. In
#'   `reround_to_fraction_level()`, the decimal level at which to round to a
#'   fraction; default is `0`.
#' @param rounding,threshold,symmetric Passed on to [`reround()`].
#'
#' @include utils.R
#'
#' @return Numeric, as long as `x`, except for the compound rounding methods,
#'   which return two values per element of `x` as in [`reround()`].
#'
#' @export
#'
#' @name fractional-rounding
#'
#' @seealso [`reround()`] and [`janitor::round_to_fraction()`].
#'
#' @examples
#' # With `denominator = 1`, this is ordinary rounding...
#' reround_to_fraction(0.4, denominator = 1, rounding = "even")
#'
#' # ...with `denominator = 2`, it rounds to the nearest half:
#' reround_to_fraction(0.4, denominator = 2, rounding = "even")
#' reround_to_fraction(0.25, denominator = 3, rounding = "even")
#'
#' # The default `rounding` returns both directions, as in `reround()`:
#' reround_to_fraction(0.25, denominator = 2)
#'
#' # `reround_to_fraction_level()` rounds to a fraction at a decimal level:
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 0)
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 1)
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 2)

reround_to_fraction <- function(
  x = NULL,
  denominator = 1,
  digits = Inf,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, denominator, digits))

  # `rounding`, `threshold`, and `symmetric` are deliberately left out. They
  # describe one rounding procedure, and `reround()` requires each of them to
  # have length 1, so including them here produced a warning about values
  # "getting paired" for arguments that cannot be paired at all -- immediately
  # followed by an error from `reround()` saying so.

  check_denominator(denominator)

  # The compound methods used to be expanded into their two constituents here,
  # because `reround()` took a vector of procedures and paired them with `x`.
  # It no longer does -- it takes one procedure and applies it to all of `x` --
  # and it has handled the compound strings itself all along, returning the two
  # results per input value interleaved. Expanding them here now errors, and
  # never did work for an `x` longer than 1 anyway: a length-2 `rounding` and a
  # longer `x` failed the length-congruence check that `reround()` used to run.

  digits <- resolve_digits_fraction(digits, denominator)

  # Recycle the three to a common length, as the check above allows. Left to
  # itself, `x * denominator` recycled only those two, and `digits` was then
  # spread over the result, so the extra values of a longer `digits` were
  # silently dropped.
  lengths_in <- lengths(list(x, denominator, digits))
  n_out <- if (any(lengths_in == 0L)) 0L else max(lengths_in)
  x <- rep_len(x, n_out)
  denominator <- rep_len(denominator, n_out)
  digits <- rep_len(digits, n_out)

  # Main part ---

  # Calculate the key result, going by the denominator:
  out <- reround(
    x = x * denominator,
    digits = 0L,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  # A compound method returns two values per input value, interleaved:
  out <- out / rep(denominator, each = length(rounding_constituents(rounding)))

  # Round all resulting values for which a number of digits has been specified
  # to that number of digits. This also proceeds as in `round_to_fraction()`,
  # except for the rounding function and its arguments.
  #
  # Two things complicate it. A compound rounding method leaves two values per
  # input value in `out`, interleaved, so `digits` -- which has one value per
  # input value -- has to be spread over them to line up. And each of the two
  # branches has to stay on its own procedure here, so that the value rounded up
  # in the step above goes on being rounded up and the one rounded down goes on
  # being rounded down. Re-applying the compound method itself would instead
  # double the values a second time.
  if (!all(is.infinite(digits))) {
    procedures <- rounding_constituents(rounding)
    n_branches <- length(procedures)

    digits <- digits |>
      rep_len(length(out) %/% n_branches) |>
      rep(each = n_branches)
    branch <- rep_len(seq_len(n_branches), length(out))

    for (b in seq_len(n_branches)) {
      i <- which(branch == b & !is.infinite(digits))
      if (length(i) > 0L) {
        out[i] <- reround(
          x = out[i],
          digits = digits[i],
          rounding = procedures[b],
          threshold = threshold,
          symmetric = symmetric
        )
      }
    }
  }

  out
}


#' @rdname fractional-rounding
#' @export

reround_to_fraction_level <- function(
  x = NULL,
  denominator = 1,
  digits = 0L,
  rounding = "up_or_down",
  threshold = 5,
  symmetric = FALSE
) {
  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(x, denominator, digits))

  # `rounding`, `threshold`, and `symmetric` are deliberately left out. They
  # describe one rounding procedure, and `reround()` requires each of them to
  # have length 1, so including them here produced a warning about values
  # "getting paired" for arguments that cannot be paired at all -- immediately
  # followed by an error from `reround()` saying so.

  check_denominator(denominator)

  digits <- resolve_digits_fraction(digits, denominator)

  # The compound methods used to be expanded into their two constituents here,
  # because `reround()` took a vector of procedures and paired them with `x`.
  # It no longer does -- it takes one procedure and applies it to all of `x` --
  # and it has handled the compound strings itself all along, returning the two
  # results per input value interleaved. Expanding them here now errors, and
  # never did work for an `x` longer than 1 anyway: a length-2 `rounding` and a
  # longer `x` failed the length-congruence check that `reround()` used to run.

  # Main part ---

  # Calculate the key result, going by the denominator:
  out <- reround(
    x = x * denominator,
    digits = digits,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )

  # Divide by the denominator, then return the result. A compound method
  # returns two values per input value, interleaved:
  out / rep(denominator, each = length(rounding_constituents(rounding)))
}


# `denominator` must be finite and at least 1. A missing value propagates to a
# missing result instead:
check_denominator <- function(denominator) {
  wrong <- (denominator < 1 | is.infinite(denominator)) %in% TRUE
  if (any(wrong)) {
    abort_in_export(
      "!" = "`denominator` must be a finite number of 1 or greater.",
      "x" = "It has {cli::qty(sum(wrong))}value{?s} \\
        {wrap_in_backticks(denominator[wrong])}."
    )
  }
}
