#' Count decimal places
#'
#' @description `decimal_places()` counts the decimal places of a numeric
#'   vector, or of a string vector coercible to numeric.
#'
#' @section Trailing zeros: A numeric value has lost its trailing zeros: `3.70`
#'   is stored as `3.7` and has one decimal place. If trailing zeros matter,
#'   enter the values as strings, such as `"3.70"`, or restore them with
#'   [`restore_zeros()`] if the original number of decimal places is known.
#'
#' @details The function counts the run of digits after the first `sep`,
#'   after trimming whitespace and applying any exponent: `"5.30%"` has two
#'   decimal places, `"1e-5"` has five, and `"1..5"` has none. Missing and
#'   non-finite values have `NA` decimal places, as do the string `"NA"` and
#'   strings that [`as.numeric()`] reads as an infinity or `NaN`.
#'
#'   Numeric values with more than 15 significant digits cannot be counted
#'   accurately; enter such numbers as strings.
#'
#' @param x Numeric, or a string coercible to numeric.
#' @param sep String. The separator between integer part and mantissa, matched
#'   literally. Default is `"."`. Only applies to strings; numbers are always
#'   written with a point.
#'
#' @return Integer. The number of decimal places of each element of `x`.
#'
#' @include utils.R
#'
#' @rdname decimal_places
#' @export
#'
#' @seealso [`decimal_places_df()`] for data frames.
#'
#' @examples
#' decimal_places(x = 2.851)
#' decimal_places(x = "2.851")
#'
#' # Trailing zeros are only counted in strings:
#' decimal_places(x = c(7.3900, "7.3900"))
#'
#' # Whitespace at the end of a string is ignored:
#' decimal_places(x = "6.0     ")

decimal_places <- function(x, sep = ".") {
  sep <- sep_literal(sep)

  # Strings are matched against the tokens rather than run through
  # `as.numeric()`, which would also reject strings like `"5.30%"`. Values
  # decided here are blanked out so that nothing below meets a missing value:
  if (is.numeric(x)) {
    non_finite <- !is.finite(x)
    x <- number_to_string(x)
    # R writes a number with a point, whatever `sep` the strings use:
    sep <- "."
  } else {
    x <- trim_space(x)
    non_finite <- x %in% c(NA, "NA") | is_non_finite_token(x)
  }
  x[non_finite] <- ""

  # An exponent moves the decimal point (`"1e-05"` has five decimal places,
  # `"1.5e3"` none), so it is split off and applied to the count below. One
  # beyond the integer range makes the count `NA`:
  pos_exponent <- regexpr("[eE][+-]?[0-9]+$", x)
  has_exponent <- pos_exponent > 0L
  exponent <- integer(length(x))
  exponent[has_exponent] <- x[has_exponent] |>
    substring(pos_exponent[has_exponent] + 1L) |>
    as.integer() |>
    suppressWarnings()
  x[has_exponent] <- substr(
    x[has_exponent],
    1L,
    pos_exponent[has_exponent] - 1L
  )

  # Only the digit run right after the first separator counts: `"1.2.3"` has
  # one decimal place. `decimal_places_scalar()` takes the same steps, and a
  # generated corpus in test-decimal-places.R holds the two together:
  pos_sep <- regexpr(sep, x, fixed = TRUE)
  mantissa <- substring(x, pos_sep + nchar(sep))
  mantissa[pos_sep < 0L] <- ""
  out <- mantissa |>
    regexpr(pattern = "^[0-9]*") |>
    attr("match.length")

  out <- pmax(out - exponent, 0L)
  out[non_finite] <- NA_integer_
  out
}


# `as.character()` uses scientific notation when it is shorter (`100000` is
# `"1e+05"`), so such numbers are written out in full here, to be counted and
# compared to other spellings. Near the ends of the double range it can use all
# 15 digits where fewer stand for the same number (`1e-300` gives
# `"9.99999999999999e-301"`), so the fewest digits that round-trip are searched
# for. The common case costs one fixed-string match:
number_to_string <- function(x) {
  out <- as.character(x)
  for (i in grep("e", out, fixed = TRUE)) {
    digits <- nchar(gsub("[^0-9]", "", sub("e.*$", "", out[i])))
    if (digits >= 15L) {
      for (digits in 1:15) {
        if (as.numeric(format(x[i], digits = digits)) == x[i]) {
          break
        }
      }
    }
    out[i] <- format(
      x[i],
      digits = digits,
      scientific = FALSE,
      decimal.mark = "."
    )
  }
  out
}


# Strings for comparing values of any type, as the `duplicate_*()` functions
# do: a number is written out in full, so that `100000L`, `100000`, and
# `"100000"` are one value, as are `0.0001` and `"0.0001"`. `NaN` is a missing
# value, as everywhere in the package, not the string `"NaN"`:
value_to_string <- function(x) {
  if (!is.numeric(x)) {
    return(as.character(x))
  }
  out <- number_to_string(x)
  out[is.nan(x)] <- NA_character_
  out
}


# Faster version of `decimal_places()` for a single value, used inside other
# single-case functions. It must agree with `decimal_places()`, which a
# generated corpus in test-decimal-places.R checks.
decimal_places_scalar <- function(x, sep = ".") {
  # Branching on `is.character()` spares a number the token check and the
  # trimming, which only a string can need:
  if (is.character(x)) {
    x <- trim_space(x)
    if (is.na(x) || x == "NA" || is_non_finite_token(x)) {
      return(NA_integer_)
    }
  } else if (is.finite(x)) {
    x <- number_to_string(x)
    sep <- "."
  } else {
    return(NA_integer_)
  }

  # See `decimal_places()` on exponents. Almost no value has one, so a
  # fixed-string search spares most of them the regular expression:
  pos_exponent <- -1L
  if (grepl("e", x, fixed = TRUE) || grepl("E", x, fixed = TRUE)) {
    pos_exponent <- regexpr("[eE][+-]?[0-9]+$", x)
  }

  exponent <- 0L
  if (pos_exponent > 0L) {
    exponent <- x |>
      substring(pos_exponent + 1L) |>
      as.integer() |>
      suppressWarnings()
    x <- substr(x, 1L, pos_exponent - 1L)
  }

  sep <- sep_literal(sep)
  pos_sep <- regexpr(sep, x, fixed = TRUE)

  out <- if (pos_sep < 0L) {
    0L
  } else {
    x |>
      substring(pos_sep + nchar(sep)) |>
      regexpr(pattern = "^[0-9]*") |>
      attr("match.length")
  }

  max(out - exponent, 0L)
}


#' Count decimal places in a data frame
#'
#' `decimal_places_df()` replaces every value in the selected columns by its
#' number of decimal places. By default, it operates on all columns coercible
#' to numeric.
#'
#' @param data Data frame.
#' @param cols Columns to operate on, selected with
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, restricted by `check_numeric_like`.
#' @param check_numeric_like Logical. If `TRUE` (the default), only columns
#'   that [`is_numeric_like()`] accepts are processed.
#' @param sep String. The separator between integer part and mantissa, matched
#'   literally. Default is `"."`.
#'
#' @return Data frame with the selected columns replaced by their counts of
#'   decimal places.
#'
#' @seealso [`decimal_places()`].
#'
#' @export
#'
#' @examples
#' iris <- iris |>
#'   tibble::as_tibble() |>
#'   dplyr::mutate(across(everything(), as.character))
#'
#' # All numeric-like columns, but not `Species`:
#' iris |>
#'   decimal_places_df()
#'
#' # Selected columns only:
#' iris |>
#'   decimal_places_df(cols = starts_with("Sepal"))

decimal_places_df <- function(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  sep = "."
) {
  sep_literal_value <- sep_literal(sep)

  # The function works on whole columns, and grouping would get in the way:
  # `dplyr::select()` re-adds the grouping columns, and `across()` runs per
  # group. The output is ungrouped:
  data <- dplyr::ungroup(data)
  quo_cols <- rlang::enquo(cols)
  names_selected <- quo_cols |>
    tidyselect::eval_select(data) |>
    names()

  # Only columns that `cols` selected are worth a warning:
  is_numeric_like_selected <- vapply(
    data[names_selected],
    is_numeric_like_col,
    logical(1L),
    sep = sep_literal_value
  )
  names_wrong_cols <- names_selected[!is_numeric_like_selected]

  if (length(names_wrong_cols) > 0L) {
    if (check_numeric_like) {
      msg_exclusion <- paste0(c("was", "were"), " excluded")
      names_selected <- names_selected[is_numeric_like_selected]
    } else {
      msg_exclusion <- c(
        "had its decimal places counted anyway",
        "had their decimal places counted anyway"
      )
    }
    warn_wrong_columns_selected(
      names_wrong_cols,
      msg_exclusion,
      msg_reason = "numeric-like",
      msg_it_they = c("It isn't", "They aren't")
    )
  }

  data |>
    dplyr::mutate(dplyr::across(
      .cols = all_of(names_selected),
      .fns = function(x) decimal_places(x = x, sep = sep)
    ))
}


# `sep` and its relatives in `restore_zeros()` are literal strings. An escaped
# character such as `"\\."`, valid when they were regular expressions, is read
# as that character.
sep_literal <- function(sep) {
  if (!is.character(sep) || length(sep) != 1L || is.na(sep)) {
    name <- deparse(substitute(sep))
    abort_in_export(
      "`{name}` must be a single string.",
      "x" = "It is {wrong_spec_string(sep)}."
    )
  }
  sub("^\\\\(.)$", "\\1", sep)
}


# `decimal_places()` and `decimal_places_scalar()` read strings through these
# two, so that they cannot drift apart.
#
# `\s` is ASCII whitespace, `"\v"` and `"\f"` included. `\p{Z}` adds Unicode
# spaces such as the no-break space, common in tables copied from PDFs.
trim_space <- function(x) {
  gsub("^[\\s\\p{Z}]+|[\\s\\p{Z}]+$", "", x, perl = TRUE)
}

# The strings `as.numeric()` reads as an infinity or `NaN`, in any letter case.
# `chartr()` folds the ASCII letters of these tokens and nothing else, so unlike
# `tolower()` it does not depend on the locale. A number too large for a double,
# such as `"1e400"`, is read as an infinity as well:
is_non_finite_token <- function(x) {
  chartr("AFINTY", "afinty", x) %in%
    NON_FINITE_TOKENS |
    is.infinite(suppressWarnings(as.numeric(x)))
}
