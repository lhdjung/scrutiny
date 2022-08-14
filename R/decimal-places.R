
#' Count decimal places
#'
#' @description `decimal_places()` counts the decimal places in a numeric
#'   vector, or in a string vector that can be coerced to numeric.
#'
#'   `decimal_places_scalar()` is much faster but only takes a single input. It
#'   is useful as a helper within other single-case functions.
#'
#' @section Trailing zeros: If trailing zeros matter, don't convert numeric
#'   values to strings: In numeric values, any trailing zeros have already been
#'   dropped, and any information about them was lost (e.g., `3.70` returns
#'   `3.7`). Enter those values as strings instead, such as `"3.70"` instead of
#'   `3.70`. However, you can restore lost trailing zeros with `restore_zeros()`
#'   if the original number of decimal places is known.
#'
#'   If you need to enter many such values as strings, consider using
#'   `tibble::tribble()` and drawing quotation marks around all values in a
#'   `tribble()` column at once via RStudio's multiple cursors.

#' @details Decimal places in numeric values can't be counted accurately if the
#'   number has 15 or more characters in total, including the integer part and
#'   the decimal point. A possible solutions is to enter the number as a string
#'   to count all digits. (Converting to string is not sufficient -- those
#'   numbers need to be *entered* in quotes.)
#'
#'   The functions ignore any whitespace at the end of a string, so they won't
#'   mistake spaces for decimal places.

#' @param x Numeric (or string that can be coerced to numeric). Object with
#'   decimal places to count.
#' @param sep Substring that separates the mantissa from the integer part.
#'   Default is `"\\."`, which renders a decimal point.
#'
#' @return Integer. Number of decimal places in `x`.
#'
#' @include utils.R

#' @seealso Rounding functions:
#' - `round_up()` and `round_down()` round up or down from 5, respectively.
#' - `round_up_from()` and `round_down_from()` allow users to specify custom
#'   thresholds for rounding up or down.
#' - `round_ceiling()` and `round_floor()` work like `ceiling()` or `floor()`,
#'   but for decimal numbers instead of just integers.
#' - `round_trunc()` and `round_anti_trunc()` always round towards zero or away
#'   from it.
#'
#' Furthermore, `unround()` takes a rounded number and returns the range of the
#' original value.

#' @examples
#' # `decimal_places()` works on both numeric values
#' # and strings...
#' decimal_places(x = 2.851)
#' decimal_places(x = "2.851")
#'
#' # ... but trailing zeros are only counted within
#' # strings:
#' decimal_places(x = c(7.3900, "7.3900"))
#'
#' # This doesn't apply to non-trailing zeros; these
#' # behave just like any other digit would:
#' decimal_places(x = c(4.08, "4.08"))
#'
#' # Whitespace at the end of a string is not counted:
#' decimal_places(x = "6.0     ")
#'
#' # `decimal_places_scalar()` is much faster,
#' # but only works with a single number or string:
#' decimal_places_scalar(x = 8.13)
#' decimal_places_scalar(x = "5.024")
#'
#'
#' @rdname decimal_places
#' @export


# Note: The internal helper functions used within `decimal_places()` can be
# found in the utils.R file, but they are essentially self-explaining. They are
# used here instead of anonymous functions because they run slightly faster.

decimal_places <- function(x, sep = "\\.") {
  out <- stringr::str_split(stringr::str_trim(x), sep, 2)
  out <- purrr::modify_if(out, !is.na(out), stringr::str_length)
  out <- purrr::modify_if(out, is_length_1_and_not_na, set_to_0)

  return(as.integer(unlist(
    purrr::map_if(out, is_length_greater_1, `[`, 2)
  )))
}




#' @rdname decimal_places
#' @export

# Faster, single-case (scalar) function to be used as a helper within other
# single-case functions:
decimal_places_scalar <- function(x, sep = "\\.") {
  if (is.na(x)) {
    return(NA)
  }
  out <- stringr::str_split(stringr::str_trim(x), sep, 2)
  out <- stringr::str_length(out[[1]][2])
  if (is.na(out)) {
    return(as.integer(0))
  }

  return(as.integer(out))
}



