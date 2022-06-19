
#' Generalized rounding to the nearest fraction of a specified denominator
#'
#' @description Two functions that round numbers to specific fractions, not just
#'   to the next higher decimal level. They are inspired by
#'   `janitor::round_to_fraction()` but feature all the options of `reround()`:
#'
#'   - `reround_to_fraction()` closely follows `janitor::round_to_fraction()`
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
#'   `ceiling(log10(denominator)) + 1`, as in `janitor::round_to_fraction()`.
#'   Default is `Inf`, in which case there is no subsequent rounding.
#'   - In `reround_to_fraction_level()`: This function will round to a fraction
#'   of the number at the decimal level specified by `digits`. Default is `0`.
#' @param rounding,threshold,symmetric More arguments passed down to
#'   `reround()`.

#' @include utils.R
#'
#' @return Numeric vector of the same length as `x` unless `rounding` is either
#'   of `"up_or_down"`, `"up_from_or_down_from"`, and `"ceiling_or_floor"`. In
#'   these cases, it will always have length 2.
#'
#' @export
#'
#' @seealso `reround()`, which the functions wrap, and
#'   `janitor::round_to_fraction()`, part of which they copy.
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


reround_to_fraction <- function(x = NULL, denominator = 1, digits = Inf,
                                rounding = "up_or_down", threshold = 5,
                                symmetric = FALSE) {

  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(
    x, denominator, digits, rounding, threshold, symmetric
  ))

  # Check whether `denominator` values are >= 1:
  if (any(denominator < 1)) {
    value_values <- dplyr::if_else(length(denominator) == 1, "value", "values")
    cli::cli_abort(c(
      "`denominator` has {value_values} {denominator[denominator < 1]}",
      "x" = "It needs to be 1 or greater."
    ))
  }

  if (any(!is.infinite(digits))) {
    digits_numeric <- digits[!is.infinite(digits)]
    if (!all(is_whole_number(digits_numeric))) {
      cli::cli_abort(c(
        "`digits` given as {digits_numeric[!is_whole_number(digits_numeric)]}",
        "x" = "Each `digit` value needs to be a whole number."
      ))
    }
  }

  if (any(rounding == "up_or_down")) {
    rounding <- c("up", "down")
  } else if (any(rounding == "up_from_or_down_from")) {
    rounding <- c("up_from", "down_from")
  } else if (any(rounding == "ceiling_or_floor")) {
    rounding <- c("ceiling", "floor")
  }


  # Main part ---

  # Calculate the key result, going by the denominator:
  out <- reround(
    x = x * denominator, digits = 0, rounding = rounding,
    threshold = threshold, symmetric = symmetric
  )
  out <- out / denominator

  # The `auto` option for `digits` is the same as in
  # `janitor::round_to_fraction()`:
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1
  }

  # Round all resulting values for which a number of digits has been specified
  # to that number of digits. This also proceeds as in `round_to_fraction()`,
  # except for the rounding function and its arguments:
  mask_inf_digits <- is.infinite(digits)

  if (!all(mask_inf_digits)) {
    out[!mask_inf_digits] <- reround(
      out, digits = digits, rounding = rounding, threshold = threshold,
      symmetric = symmetric
    )
  }

  # Finally, return the results:
  out

}



#' @rdname reround_to_fraction
#' @export

reround_to_fraction_level <- function(x = NULL, denominator = 1, digits = 0,
                                      rounding = "up_or_down", threshold = 5,
                                      symmetric = FALSE) {

  # Checks ---

  # If any two arguments called right below are length > 1, they need to have
  # the same length. Otherwise, the call will fail. But even so, there will be a
  # warning that values will get paired:
  check_lengths_congruent(list(
    x, denominator, digits, rounding, threshold, symmetric
  ))

  if (any(denominator < 1)) {
    value_values <- dplyr::if_else(length(denominator) == 1, "value", "values")
    cli::cli_abort(c(
      "`denominator` has {value_values} {denominator[denominator < 1]}",
      "x" = "It needs to be 1 or greater."
    ))
  }

  # The `auto` option for `digits` is the same as in
  # `janitor::round_to_fraction()`:
  if (identical(digits, "auto")) {
    digits <- ceiling(log10(denominator)) + 1
  }

  # Check whether `digit` values are whole numbers:
  if (any(!is.infinite(digits))) {
    digits_numeric <- digits[!is.infinite(digits)]
    if (!all(is_whole_number(digits_numeric))) {
      cli::cli_abort(c(
        "`digits` given as {digits_numeric[!is_whole_number(digits_numeric)]}",
        "x" = "Each `digit` value needs to be a whole number."
      ))
    }
  }

  if (any(rounding == "up_or_down")) {
    rounding <- c("up", "down")
  } else if (any(rounding == "up_from_or_down_from")) {
    rounding <- c("up_from", "down_from")
  } else if (any(rounding == "ceiling_or_floor")) {
    rounding <- c("ceiling", "floor")
  }


  # Main part ---

  # Calculate the key result, going by the denominator:
  out <- reround(
    x = x * denominator, digits = digits, rounding = rounding,
    threshold = threshold, symmetric = symmetric
  )

  # Divide by the denominator, then return the result:
  out / denominator
}


