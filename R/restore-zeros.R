#' Restore trailing zeros
#'
#' @description `restore_zeros()` takes a vector with values that might have
#'   lost trailing zeros, most likely from being registered as numeric. It turns
#'   each value into a string and adds trailing zeros until the mantissa hits
#'   some limit.
#'
#'   The default for that limit is the number of digits in the longest mantissa
#'   of the vector's values. The length of the integer part plays no role.
#'
#'   Don't rely on the default limit without checking: The original width could
#'   have been larger because the longest extant mantissa might itself have lost
#'   trailing zeros.
#'
#'   `restore_zeros_df()` is a variant for data frames. It wraps
#'   `restore_zeros()` and, by default, applies it to all columns that are
#'   coercible to numeric.

#' @details These functions exploit the fact that groups of summary values such
#'   as means or percentages are often reported to the same number of decimal
#'   places. If such a number is known but values were not entered as strings,
#'   trailing zeros will be lost. In this case, `restore_zeros()` or
#'   `restore_zeros_df()` will be helpful to prepare data for consistency
#'   testing functions such as [`grim_map()`] or [`grimmer_map()`].

#' @section Displaying decimal places: You might not see all decimal places of
#'   numeric values in a vector, and consequently wonder if `restore_zeros()`,
#'   when applied to the vector, adds too many zeros. That is because displayed
#'   numbers, unlike stored numbers, are often rounded.
#'
#'   For a vector `x`, you can count the characters of the longest mantissa from
#'   among its values like this:
#'
#'   `x |> decimal_places() |> max()`
#'
#' @param x Numeric (or string coercible to numeric). Vector of numbers that
#'   might have lost trailing zeros. A value that is not coercible to numeric,
#'   such as `"5%"`, is returned as `NA`.
#' @param width Integer. Number of decimal places the mantissas should have,
#'   including the restored zeros. If specified, `width` needs to be length 1 or
#'   the same length as `x`. Default is `NULL`, in which case the number of
#'   characters in the longest mantissa will be used instead.
#' @param sep_in String. The literal separator between the input's integer
#'   part and its mantissa. Default is `"."`. (The former default, the regular
#'   expression `"\\."`, is still read as a decimal point.)
#' @param sep_out String. The literal separator that will be returned in the
#'   output between the integer part and the mantissa. By default, `sep_out` is
#'   the same as `sep_in`.
#' @param check_width String (length 1). What to do if `width` was specified but
#'   some values in `x` have more decimal places (e.g., `x = 0.123, width = 2`)?
#'   The default, `"capped"`, is to throw an error. Use `check_width = "never"`
#'   to avoid this.
#' @param data Data frame or matrix. Only in `restore_zeros_df()`, and instead
#'   of `x`.
#' @param cols Only in `restore_zeros_df()`. Select columns from `data` using
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, which selects all columns that pass the test of
#'   `check_numeric_like`.
#' @param check_numeric_like Logical. Only in `restore_zeros_df()`. If `TRUE`
#'   (the default), the function will skip columns that are not numeric or
#'   coercible to numeric, as determined by [`is_numeric_like()`].
#' @param check_decimals Logical. Only in `restore_zeros_df()`. If set to
#'   `TRUE`, the function will skip columns where no values have any decimal
#'   places. Default is `FALSE`.
#' @param ... Only in `restore_zeros_df()`. These dots must be empty.

#' @return
#' - For `restore_zeros()`, a string vector. At least some of the strings
#'   will have newly restored zeros, unless (1) all input values had the same
#'   number of decimal places, and (2) `width` was not specified as a number
#'   greater than that single number of decimal places.
#' - For `restore_zeros_df()`, a data frame.
#'
#' @export
#'
#' @include utils.R
#'
#' @seealso Wrapped functions: [`sprintf()`].
#'
#' @examples
#' # By default, the target width is that of
#' # the longest mantissa:
#' vec <- c(212, 75.38, 4.9625)
#' vec |>
#'   restore_zeros()
#'
#' # Alternatively, supply a number via `width`:
#' vec |>
#'   restore_zeros(width = 6)
#'
#' # For better printing:
#' iris <- tibble::as_tibble(iris)
#'
#' # Apply `restore_zeros()` to all numeric
#' # columns, but not to the factor column:
#' iris |>
#'   restore_zeros_df()
#'
#' # Select columns as in `dplyr::select()`:
#' iris |>
#'   restore_zeros_df(starts_with("Sepal"), width = 3)

restore_zeros <- function(
  x,
  width = NULL,
  sep_in = ".",
  sep_out = sep_in,
  check_width = c("capped", "never")
) {
  check_width <- rlang::arg_match(check_width)
  sep_in <- sep_literal(sep_in)

  # Make sure no whitespace (from values that already were strings) is factored
  # into the count:
  x <- stringr::str_trim(x)

  # From here on, the separator is a decimal point, so that `as.numeric()` can
  # read `x`. It is put back in at the end:
  if (sep_in != ".") {
    x <- sub(sep_in, ".", x, fixed = TRUE)
  }

  # A value that is not a number has no zeros to restore. It becomes missing,
  # like a missing value, rather than being padded into nonsense such as
  # `"5%000"` -- or, as it used to, into the string `"NA"`:
  # The string `"NA"` spells out a missing value, so it is not a non-number:
  x_num <- suppressWarnings(as.numeric(x))
  not_number <- is.na(x_num) & !(x %in% c(NA, "NA"))
  if (any(not_number)) {
    cli::cli_warn(c(
      "Values that are not numbers became `NA`.",
      "x" = "This concerns {wrap_in_backticks(unique(x[not_number]))}."
    ))
  }
  x[is.na(x_num)] <- NA_character_

  # R writes small and large numbers in scientific notation by itself --
  # `as.character(0.0001)` is `"1e-04"` -- and zeros appended to that would
  # multiply the value instead of padding it. Such values are written out in
  # full, to the decimal places that `decimal_places()` reads off the exponent.
  # Other unusual spellings that `as.numeric()` accepts, such as `".5"`,
  # `"+1.5"`, or `"0x1A"`, are written out plainly the same way:
  rewrite <- is.finite(x_num) & !grepl("^-?[0-9]+(\\.[0-9]*)?$", x)
  x[rewrite] <- sprintf("%.*f", decimal_places(x[rewrite]), x_num[rewrite])

  # Count the decimal places. This is `NA` where there are none to count: in a
  # missing value and in an infinity.
  width_mantissa <- decimal_places(x)

  # Determine the maximal width to which the mantissas should be padded in
  # accordance with the `width` argument, the default of which, `NULL`, makes
  # the function go by the maximal length of already-present mantissas:
  if (is.null(width)) {
    # Throw a warning if `x` can't be formatted with the given arguments:
    if (length(x) == 1L) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "`x` has length 1",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    } else if (all(width_mantissa == 0L, na.rm = TRUE)) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "None of the {length(x)} `x` values has any decimal places.",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    }
    # The number of decimal places to which `x` values will be padded with zeros
    # is determined by the number of characters in the longest mantissa...
    width_target <- max(0L, width_mantissa, na.rm = TRUE)
    # ... unless the user manually specified that target number via `width`.
    # This is an error if `width` is not a single integer-ish number or a vector
    # of such numbers with the same length as `x`...
  } else if (
    !any(c(1L, length(x)) == length(width)) || !all(is_whole_number(width))
  ) {
    cli::cli_abort(c(
      "`width` must be a single, whole number \
      (or a vector of whole numbers with the same length as `x`).",
      "x" = "It is {width}."
    ))
    # ... or if any `x` elements have more decimal places than `width` allows:
  } else if (
    check_width == "capped" && any(width_mantissa > width, na.rm = TRUE)
  ) {
    offenders <- x[which(width_mantissa > width)]
    cli::cli_abort(c(
      "Some values have more decimal places than `width` foresees.",
      "x" = "`width` was set to {width}.",
      "i" = "Values with more than {width} decimal place{?s}:",
      "i" = "{offenders}",
      ">" = "Avoid this error with `check_width = \"never\"`."
    ))
  } else {
    width_target <- width
  }

  # Pad `x` with the missing number of zeros, appended literally. Formatting
  # with `sprintf("%.*f")` instead would print the binary expansion of the
  # value: `0.1` padded to 20 places came out as `"0.10000000000000000555"`. A
  # whole number gets a decimal point first:
  n_zeros <- width_target - width_mantissa
  pad <- !is.na(n_zeros) & n_zeros > 0L
  point <- dplyr::if_else(grepl(".", x[pad], fixed = TRUE), "", ".")
  out <- x
  out[pad] <- paste0(x[pad], point, strrep("0", n_zeros[pad]))

  # By default, the separator in the output vector is the same as in the input,
  # but it might have been overridden via `sep_out`:
  if (sep_literal(sep_out) == ".") {
    out
  } else {
    sub(".", sep_literal(sep_out), out, fixed = TRUE)
  }
}


#' @rdname restore_zeros
#' @export

restore_zeros_df <- function(
  data,
  cols = everything(),
  check_numeric_like = TRUE,
  check_decimals = FALSE,
  width = NULL,
  sep_in = ".",
  sep_out = sep_in,
  check_width = c("capped", "never"),
  ...
) {
  # Check whether the user specified any "old" arguments: those starting on a
  # dot. This check is now the only remaining purpose of the `...` dots because
  # these are no longer meant to be used. Any other arguments passed through
  # them should still lead to an error:
  check_new_args_without_dots(
    data,
    dots = rlang::enquos(...),
    old_args = c(
      ".data",
      ".check_decimals",
      ".width",
      ".sep_in",
      ".sep_out",
      ".sep"
    ),
    name_fn = "restore_zeros_df"
  )

  # Check that `data` is a data frame or matrix:
  if (!is.data.frame(data)) {
    if (is.matrix(data)) {
      data <- tibble::as_tibble(data, .name_repair = "unique")
    } else {
      cli::cli_abort(c(
        "!" = "`data` must be a data frame (or a matrix).",
        "x" = "It is {an_a_type(data)}.",
        ">" = "Did you mean `restore_zeros()`, without `_df`?"
      ))
    }
  }

  # Names of selection-suitable columns. A column with comma decimals is only
  # numeric-like when read with `sep_in`:
  sep_in_literal <- sep_literal(sep_in)
  names_num_cols <- data |>
    dplyr::select(where(function(x) is_numeric_like_col(x, sep_in_literal))) |>
    colnames()

  # By default, selection is restricted to columns that are numeric or coercible
  # to numeric. This is checked with an internal helper from the utils.R file:
  if (check_numeric_like) {
    selection2 <- rlang::expr(all_of(names_num_cols))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  # If desired by the user, create an additional selection criterion: In each
  # numeric-like column, at least one value must have at least one decimal
  # place. Otherwise...
  if (check_decimals) {
    selection3 <- rlang::expr(where(function(x) {
      !is_numeric_like_col(x, sep_in_literal) ||
        any(decimal_places(x, sep = sep_in) > 0L, na.rm = TRUE)
    }))
  } else {
    # ... the new variable is set up to be evaluated as `everything()`, which is
    # an identity element of the `&` operator in tidyselect:
    selection3 <- rlang::expr(dplyr::everything())
  }

  # Column selection is outsourced here so that the result can also be used in a
  # check below:
  cols_to_select <- rlang::expr({{ cols }} & !!selection2 & !!selection3)
  cols_to_select <- tidyselect::eval_select(cols_to_select, data)

  # Check whether any selected columns are not numeric-like, which is only
  # possible with `check_numeric_like = FALSE`. `restore_zeros()` replaces their
  # non-numeric values by `NA`. If so...
  names_cols_select <- names(cols_to_select)
  names_wrong_cols <- names_cols_select[!names_cols_select %in% names_num_cols]

  # ...the user is warned:
  if (length(names_wrong_cols) > 0L) {
    warn_wrong_columns_selected(
      names_wrong_cols,
      msg_exclusion = c(
        "had its non-numeric values replaced by `NA`",
        "had their non-numeric values replaced by `NA`"
      ),
      msg_reason = "numeric-like",
      msg_it_they = c("It isn't", "They aren't")
    )
  }

  # By default, a columns is selected if and only if it's numeric-like.
  # Additional constrains might come via `selection2` or `selection3` (see
  # `cols_to_select` above; by default, only `selection2` takes effect). The
  # `.fns` argument uses an anonymous function to pass on all the named
  # arguments to `restore_zeros()`:
  data |>
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(cols_to_select),
        .fns = function(data_dummy) {
          restore_zeros(
            x = data_dummy,
            width = width,
            sep_in = sep_in,
            sep_out = sep_out,
            check_width = check_width
          )
        }
      )
    )
}
