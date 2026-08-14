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
#'   `3.70`. However, you can restore lost trailing zeros with
#'   [`restore_zeros()`] if the original number of decimal places is known.
#'
#'   If you need to enter many such values as strings, consider using
#'   [`tibble::tribble()`] and drawing quotation marks around all values in a
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
#' @details Both functions count the run of digits that immediately follows the
#'   first `sep`, after removing surrounding whitespace and applying any
#'   exponent: `"5.30%"` has two decimal places, and `"1e-5"` has five. They
#'   always agree with each other; `decimal_places_scalar()` is the faster one,
#'   and `decimal_places()` is the one that takes a vector.
#'
#' @include utils.R
#'
#' @rdname decimal_places
#' @export

#' @seealso [`decimal_places_df()`], which applies `decimal_places()` to all
#'   numeric-like columns in a data frame.

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

decimal_places <- function(x, sep = "\\.") {
  x <- stringr::str_trim(x)

  # Scientific notation moves the decimal point, so the digits after `sep` are
  # not the decimal places of the number: `1e-05` has five of them and none
  # after a point, `1.5e3` has none and one after the point. R writes numerics
  # that way by itself -- `as.character(0.0001)` is `"1e-04"` -- so this is not
  # only about strings the user typed. The exponent is split off here and
  # applied to the count below:
  exponent <- suppressWarnings(as.integer(
    stringr::str_match(x, "[eE]([+-]?[0-9]+)$")[, 2L]
  ))
  x <- stringr::str_remove(x, "[eE][+-]?[0-9]+$")

  # Only the run of digits that immediately follows the separator counts, not
  # every character after it: `"5.30%"` has two decimal places, not three, and
  # `"1.2.3"` has one, not three. This is the same rule that
  # `decimal_places_scalar()` applies, and the two are checked against each
  # other over a generated corpus in `test-decimal-places.R`. For a
  # well-formed number the two rules agree anyway, because every character of
  # its mantissa is a digit. `str_split_fixed()` returns an empty mantissa
  # where there is no separator, and `regexpr()` counts the digit run for the
  # whole vector in one pass:
  mantissa <- stringr::str_split_fixed(x, sep, n = 2L)[, 2L]
  out <- attr(regexpr("^[0-9]*", mantissa), "match.length")

  # `str_split_fixed()` gives a missing value an empty mantissa rather than a
  # missing one, so it would otherwise count as zero decimal places:
  out[is.na(x)] <- NA_integer_

  # A value without an exponent is shifted by nothing, and a positive exponent
  # can only cancel decimal places, never create negative ones:
  exponent[is.na(exponent)] <- 0L
  out <- out - exponent
  out[!is.na(out) & out < 0L] <- 0L
  out
}


#' @rdname decimal_places
#' @export

# Faster, single-case (scalar) function to be used as a helper within other
# single-case functions:
decimal_places_scalar <- function(x, sep = "\\.") {
  if (is.na(x)) {
    return(NA_integer_)
  }

  # Whitespace must go before anything else is read off the string, exactly as
  # in `decimal_places()`: the exponent is matched at the end of the string, so
  # a single trailing space used to hide it and `"1.5e3 "` came out as 1 rather
  # than 0. Only a string the user typed can carry whitespace -- and only that
  # case pays for `trimws()` -- because `as.character()` never produces any:
  x <- if (is.character(x)) trimws(x) else as.character(x)

  # See the comment in `decimal_places()`: an exponent shifts the decimal point,
  # so it has to be split off before the digits after `sep` are counted. This is
  # what makes `decimal_places_scalar(1e-04)` 4 rather than 0, and hence what
  # keeps the step size in `seq_disperse()` and friends on the intended decimal
  # level for values that R writes in scientific notation:
  exponent <- 0L
  hit_exponent <- regmatches(x, regexpr("[eE][+-]?[0-9]+$", x))

  if (length(hit_exponent) > 0L) {
    exponent <- as.integer(sub("^[eE]", "", hit_exponent))
    x <- sub("[eE][+-]?[0-9]+$", "", x)
  }

  hit <- regmatches(x, regexpr(paste0("(?<=", sep, ")\\d+"), x, perl = TRUE))

  out <- if (length(hit) == 0L) {
    0L
  } else {
    nchar(hit)
  }

  max(out - exponent, 0L)
}


#' Count decimal places in a data frame
#'
#' For every value in a column, `decimal_places_df()` counts its decimal places.
#' By default, it operates on all columns that are coercible to numeric.
#'
#' @param data Data frame.
#' @param cols Select columns from `data` using
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, but restricted by `check_numeric_like`.
#' @param check_numeric_like Logical. If `TRUE` (the default), the function only
#'   operates on numeric columns and other columns coercible to numeric, as
#'   determined by [`is_numeric_like()`].
#' @param sep Substring that separates the mantissa from the integer part.
#'   Default is `"\\."`, which renders a decimal point.
#'
#' @return Data frame. The values of the selected columns are replaced by the
#'   numbers of their decimal places.
#'
#' @seealso Wrapped functions: [`decimal_places()`], [`dplyr::across()`].
#'
#' @export
#'
#' @examples
#' # Coerce all columns to string:
#' iris <- iris |>
#'   tibble::as_tibble() |>
#'   dplyr::mutate(across(everything(), as.character))
#'
#' # The function will operate on all
#' # numeric-like columns but not on `"Species"`:
#' iris |>
#'   decimal_places_df()
#'
#' # Operate on some select columns only
#' # (from among the numeric-like columns):
#' iris |>
#'   decimal_places_df(cols = starts_with("Sepal"))

decimal_places_df <- function(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  sep = "\\."
) {
  if (check_numeric_like) {
    selection2 <- rlang::expr(where(is_numeric_like))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  names_of_numeric_like_cols <- data |>
    dplyr::select(where(is_numeric_like)) |>
    colnames()

  data_names <- colnames(data)

  if (!identical(names_of_numeric_like_cols, data_names)) {
    names_wrong_cols <- data_names[!data_names %in% names_of_numeric_like_cols]
    if (check_numeric_like) {
      msg_exclusion <- paste0(c("was", "were"), " excluded")
    } else {
      msg_exclusion <- "didn't have any decimal places counted"
    }
    warn_wrong_columns_selected(
      names_wrong_cols,
      msg_exclusion,
      msg_reason = "numeric-like",
      msg_it_they = c("It isn't", "They aren't")
    )
  }

  dplyr::mutate(
    data,
    dplyr::across(
      .cols = {{ cols }} & !!selection2,
      .fns = function(x) decimal_places(x = x, sep = sep)
    )
  )
}
