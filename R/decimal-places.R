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
#' @param sep String. The literal separator between the integer part and the
#'   mantissa. Default is `"."`. (The former default, the regular expression
#'   `"\\."`, is still read as a decimal point.)
#'
#' @return Integer. Number of decimal places in `x`.
#'
#' @details Both functions count the run of digits that immediately follows the
#'   first `sep`, after removing surrounding whitespace and applying any
#'   exponent: `"5.30%"` has two decimal places, `"1e-5"` has five, and
#'   `"1..5"` has none. The strings that [`as.numeric()`] reads as an infinity
#'   or `NaN`, in any letter case, have `NA` decimal places, as do missing
#'   values. The two functions always agree with each other;
#'   `decimal_places_scalar()` is the faster one, and `decimal_places()` is the
#'   one that takes a vector.
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

decimal_places <- function(x, sep = ".") {
  sep <- sep_literal(sep)

  # `NaN` and the infinities have no decimal places, and `NaN` is a missing value
  # everywhere else in the package. A numeric vector is decided by `is.finite()`.
  # A character vector can only be matched against the tokens: `as.numeric()`
  # would also swallow strings like `"5.30%"`, whose two decimal places are the
  # documented answer. The values decided here are blanked out, so that nothing
  # below has to deal with a missing value:
  if (is.numeric(x)) {
    non_finite <- !is.finite(x)
    x <- as.character(x)
  } else {
    x <- trim_space(x)
    non_finite <- is.na(x) | is_non_finite_token(x)
  }
  x[non_finite] <- ""

  # Scientific notation moves the decimal point, so the digits after `sep` are
  # not the decimal places of the number: `1e-05` has five of them and none
  # after a point, `1.5e3` has none and one after the point. R writes numerics
  # that way by itself -- `as.character(0.0001)` is `"1e-04"` -- so this is not
  # only about strings the user typed. The exponent is split off here and
  # applied to the count below. One beyond the integer range is `NA`, and so is
  # the count:
  pos_exponent <- regexpr("[eE][+-]?[0-9]+$", x)
  has_exponent <- pos_exponent > 0L
  exponent <- integer(length(x))
  exponent[has_exponent] <- suppressWarnings(as.integer(
    substring(x[has_exponent], pos_exponent[has_exponent] + 1L)
  ))
  x[has_exponent] <- substr(
    x[has_exponent],
    1L,
    pos_exponent[has_exponent] - 1L
  )

  # Only the run of digits immediately after the first separator counts, not
  # every character after it: `"5.30%"` has two decimal places, `"1.2.3"` one,
  # and `"1..5"` none. Same steps as in `decimal_places_scalar()`; a generated
  # corpus in `test-decimal-places.R` holds the two together:
  pos_sep <- regexpr(sep, x, fixed = TRUE)
  mantissa <- substring(x, pos_sep + nchar(sep))
  mantissa[pos_sep < 0L] <- ""
  out <- attr(regexpr("^[0-9]*", mantissa), "match.length")

  # A positive exponent can only cancel decimal places, never create negative
  # ones:
  out <- pmax(out - exponent, 0L)
  out[non_finite] <- NA_integer_
  out
}


#' @rdname decimal_places
#' @export

# Faster, single-case (scalar) function to be used as a helper within other
# single-case functions:
decimal_places_scalar <- function(x, sep = ".") {
  # The three ways of having no decimal places to count -- a missing value, an
  # infinity, and the strings that spell one. Branching on `is.character()`
  # keeps a numeric value from paying for a check only a string can fail, and
  # from being trimmed: only a string the user typed can carry whitespace.
  # `decimal_places()` must agree with this; a generated corpus in
  # test-decimal-places.R holds the two together.
  if (is.character(x)) {
    x <- trim_space(x)
    if (is.na(x) || is_non_finite_token(x)) {
      return(NA_integer_)
    }
  } else if (is.finite(x)) {
    x <- as.character(x)
  } else {
    # `is.finite()` is `FALSE` for `NA` and `NaN` as well as the infinities:
    return(NA_integer_)
  }

  # See the comment in `decimal_places()`: an exponent shifts the decimal point,
  # so it is split off before the digits after `sep` are counted. That makes
  # `decimal_places_scalar(1e-04)` 4 rather than 0, keeping `seq_disperse()` and
  # friends on the intended decimal level. Almost no value has an exponent, so
  # a fixed-string search spares most of them the regular expression:
  pos_exponent <- -1L
  if (grepl("e", x, fixed = TRUE) || grepl("E", x, fixed = TRUE)) {
    pos_exponent <- regexpr("[eE][+-]?[0-9]+$", x)
  }

  exponent <- 0L
  if (pos_exponent > 0L) {
    exponent <- suppressWarnings(as.integer(substring(x, pos_exponent + 1L)))
    x <- substr(x, 1L, pos_exponent - 1L)
  }

  # Only the digit run right after the *first* separator counts, as in
  # `decimal_places()`. The separator is a literal string, so it is found
  # without a regular expression:
  sep <- sep_literal(sep)
  pos_sep <- regexpr(sep, x, fixed = TRUE)

  out <- if (pos_sep < 0L) {
    0L
  } else {
    attr(regexpr("^[0-9]*", substring(x, pos_sep + nchar(sep))), "match.length")
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
#' @param sep String. The literal separator between the integer part and the
#'   mantissa. Default is `"."`. (The former default, the regular expression
#'   `"\\."`, is still read as a decimal point.)
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
  sep = "."
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


# `sep` and its relatives in `restore_zeros()` are literal strings. They used to
# be regular expressions, documented as substrings, so `sep = "."` matched any
# character. The former default, the regex `"\\."`, is still read as a point.
sep_literal <- function(sep) {
  if (identical(sep, "\\.")) "." else sep
}


# `decimal_places()` and `decimal_places_scalar()` read strings through these
# two, so that they cannot drift apart on which strings they count.
#
# PCRE's `[[:space:]]` is ASCII whitespace in any locale: exactly what
# `as.numeric()` skips around a number, `"\v"` and `"\f"` included.
trim_space <- function(x) {
  gsub("^[[:space:]]+|[[:space:]]+$", "", x, perl = TRUE)
}

# The strings `as.numeric()` reads as an infinity or `NaN`, in any letter case.
# `chartr()` folds the ASCII letters of these tokens and nothing else, so unlike
# `tolower()` it does not depend on the locale:
is_non_finite_token <- function(x) {
  chartr("AFINTY", "afinty", x) %in% NON_FINITE_TOKENS
}
