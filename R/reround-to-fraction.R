#' Generalized rounding to the nearest fraction of a specified denominator
#'
#' @description Two functions that round numbers to specific fractions, not just
#'   to the next higher decimal level. They are inspired by
#'   [`janitor::round_to_fraction()`] but feature all the options of
#'   [`reround()`]:
#'
#'   - `reround_to_fraction()` closely follows [`janitor::round_to_fraction()`]
#'   by first rounding to fractions of a whole number, then optionally rounding
#'   the result to a specific number of digits in the usual way.
#'   - `reround_to_fraction_level()` rounds to the nearest fraction of a number
#'   at the specific decimal level (i.e., number of digits), without subsequent
#'   rounding. This is closer to conventional rounding functions.

#' @param x Numeric. Vector of numbers to be rounded.
#' @param denominator Numeric (>= 1) . `x` will be rounded to the nearest
#'   fraction of `denominator`. Default is `1`.
#' @param digits Numeric (whole numbers).
#'   - In `reround_to_fraction()`: If `digits` is specified, the values
#'   resulting from fractional rounding will subsequently be rounded to that
#'   many decimal places. If set to `"auto"`, it internally becomes
#'   `ceiling(log10(denominator)) + 1`, as in [`janitor::round_to_fraction()`].
#'   Default is `Inf`, in which case there is no subsequent rounding.
#'   - In `reround_to_fraction_level()`: This function will round to a fraction
#'   of the number at the decimal level specified by `digits`. Default is `0`.
#' @param rounding,threshold,symmetric More arguments passed down to
#'   [`reround()`].

#' @include utils.R
#'
#' @return Numeric vector of the same length as `x`, except for the three
#'   compound rounding methods `"up_or_down"`, `"up_from_or_down_from"`, and
#'   `"ceiling_or_floor"`, which return two values per element of `x` -- the
#'   result of each of their two constituent procedures -- so that the return
#'   value has length `2 * length(x)`. As in [`reround()`], the two values of a
#'   compound method stay next to each other.
#'
#'   (The length was previously documented as "always 2" for the compound
#'   methods, which held only for an `x` of length 1. For a longer `x`, the two
#'   procedures used to be *paired* with the elements of `x` instead of both
#'   being applied to each of them, so `reround_to_fraction(c(0.4, 0.6),
#'   denominator = 2)` returned two values, the first rounded up and the second
#'   down. It now returns all four.)
#'
#' @export
#'
#' @name fractional-rounding
#'
#' @seealso [`reround()`], which the functions wrap, and
#'   [`janitor::round_to_fraction()`], part of which they copy.
#'
#' @examples
#' #`reround_to_fraction()` rounds `0.4`
#' # to `0` if `denominator` is `1`, which
#' # is the usual integer rounding...
#' reround_to_fraction(0.4, denominator = 1, rounding = "even")
#'
#' # ...but if `denominator` is `2`, it rounds to the nearest
#' # fraction of 2, which is `0.5`:
#' reround_to_fraction(0.4, denominator = 2, rounding = "even")
#'
#' # Likewise with fractions of 3:
#' reround_to_fraction(0.25, denominator = 3, rounding = "even")
#'
#' # The default for `rounding` is to round
#' # both up and down, as in `reround()`:
#' reround_to_fraction(0.4, denominator = 2)
#'
#' # These two rounding procedures differ
#' # at the tie points:
#' reround_to_fraction(0.25, denominator = 2)
#'
#' # `reround_to_fraction_level()`, in contrast,
#' # uses `digits` to determine some decimal level,
#' # and then rounds to the closest fraction at
#' # that level:
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 0)
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 1)
#' reround_to_fraction_level(0.12345, denominator = 2, digits = 2)

#' @rdname fractional-rounding
#' @export

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

  # Check whether `denominator` values are >= 1:
  if (any(denominator < 1)) {
    value_values <- dplyr::if_else(length(denominator) == 1L, "value", "values")
    cli::cli_abort(c(
      "!" = "`denominator` must be 1 or greater.",
      "x" = "It has {value_values} {denominator[denominator < 1]}."
    ))
  }

  # The compound methods used to be expanded into their two constituents here,
  # because `reround()` took a vector of procedures and paired them with `x`.
  # It no longer does -- it takes one procedure and applies it to all of `x` --
  # and it has handled the compound strings itself all along, returning the two
  # results per input value interleaved. Expanding them here now errors, and
  # never did work for an `x` longer than 1 anyway: a length-2 `rounding` and a
  # longer `x` failed the length-congruence check that `reround()` used to run.

  # The `auto` option for `digits` is the same as in
  # `janitor::round_to_fraction()`. It has to be resolved before the check
  # below, which is numeric: `is.infinite("auto")` is `FALSE`, so the string
  # went straight into `is_whole_number()` and failed there with "non-numeric
  # argument to mathematical function". `reround_to_fraction_level()` has
  # always had these two in this order.
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1L
  }

  # Check whether `digits` values are whole numbers:
  if (!all(is.infinite(digits))) {
    digits_numeric <- digits[!is.infinite(digits)]
    if (!all(is_whole_number(digits_numeric))) {
      cli::cli_abort(c(
        "!" = "Each `digits` value must be a whole number.",
        "x" = "`digits` was given as \\
        {digits_numeric[!is_whole_number(digits_numeric)]}."
      ))
    }
  }

  # Main part ---

  # Calculate the key result, going by the denominator:
  out <- reround(
    x = x * denominator,
    digits = 0L,
    rounding = rounding,
    threshold = threshold,
    symmetric = symmetric
  )
  out <- out / denominator

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

    digits <- rep(
      rep_len(digits, length(out) %/% n_branches),
      each = n_branches
    )
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

  if (any(denominator < 1)) {
    value_values <- dplyr::if_else(length(denominator) == 1L, "value", "values")
    cli::cli_abort(c(
      "!" = "`denominator` must be 1 or greater.",
      "x" = "It has {value_values} \\
      {wrap_in_backticks(denominator[denominator < 1])}."
    ))
  }

  # The `auto` option for `digits` is the same as in
  # `janitor::round_to_fraction()`:
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1L
  }

  # Check whether `digit` values are whole numbers:
  if (!all(is.infinite(digits))) {
    digits_numeric <- digits[!is.infinite(digits)]
    if (!all(is_whole_number(digits_numeric))) {
      cli::cli_abort(c(
        "!" = "Each `digits` value must be a whole number.",
        "x" = "`digits` was given as \\
        {digits_numeric[!is_whole_number(digits_numeric)]}."
      ))
    }
  }

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

  # Divide by the denominator, then return the result:
  out / denominator
}
