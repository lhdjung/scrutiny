#' Restore trailing zeros
#'
#' @description `restore_zeros()` formats numbers as strings with a given number
#'   of decimal places, padding with zeros. By default, the target is the number
#'   of decimal places of the value that has the most. Do not rely on this
#'   default without checking it, since the longest mantissa may itself have
#'   lost trailing zeros.
#'
#'   `restore_zeros_df()` applies it to the columns of a data frame, by default
#'   all those coercible to numeric.
#'
#' @details Groups of reported values usually share a number of decimal places.
#'   If that number is known but the values were entered as numbers, these
#'   functions put the trailing zeros back.
#'
#'   Displayed numbers are often rounded, so a value may have more decimal
#'   places than it shows. `x |> decimal_places() |> max()` gives the actual
#'   maximum.
#'
#'   As in [`decimal_places()`], numeric values with more than 15 significant
#'   digits cannot be padded accurately; enter such numbers as strings.
#'
#' @param x Numeric, or a string coercible to numeric. A value that is not
#'   coercible, such as `"5%"`, becomes `NA` with a warning, and so does one
#'   with an exponent so large that it can't be written out, such as
#'   `"1e-999999"`.
#' @param width Integer. Number of decimal places the output should have, up
#'   to 100,000. Length 1 or the length of `x`. Default is `NULL`, the largest number of
#'   decimal places in `x`.
#' @param sep_in String. The separator between integer part and mantissa in
#'   the input, matched literally. Default is `"."`.
#' @param sep_out String. The separator in the output. Default is `sep_in`.
#' @param check_width String (length 1). What to do if a value has more decimal
#'   places than `width`? `"capped"` (the default) throws an error; `"never"`
#'   lets it pass.
#' @param data Data frame or matrix. Only in `restore_zeros_df()`.
#' @param cols Columns to operate on, selected with
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, restricted by `check_numeric_like`. Only in
#'   `restore_zeros_df()`.
#' @param check_numeric_like Logical. If `TRUE` (the default), only columns
#'   that [`is_numeric_like()`] accepts are processed. Only in
#'   `restore_zeros_df()`.
#' @param check_decimals Logical. If `TRUE`, columns without any decimal places
#'   are skipped. Default is `FALSE`. Only in `restore_zeros_df()`.
#'
#' @return
#' - For `restore_zeros()`, a string vector.
#' - For `restore_zeros_df()`, a data frame.
#'
#' @export
#'
#' @include utils.R
#'
#' @examples
#' vec <- c(212, 75.38, 4.9625)
#'
#' # By default, pad to the longest mantissa:
#' restore_zeros(vec)
#'
#' # Or to a given width:
#' restore_zeros(vec, width = 6)
#'
#' iris <- tibble::as_tibble(iris)
#'
#' # All numeric columns, but not the factor:
#' restore_zeros_df(iris)
#'
#' # Selected columns:
#' restore_zeros_df(iris, starts_with("Sepal"), width = 3)

restore_zeros <- function(
  x,
  width = NULL,
  sep_in = ".",
  sep_out = sep_in,
  check_width = c("capped", "never")
) {
  check_width <- rlang::arg_match(check_width)
  sep_in <- sep_literal(sep_in)

  # A number is written out in full, as `decimal_places()` reads it:
  x <- if (is.numeric(x)) number_to_string(x) else stringr::str_trim(x)

  # Use a point so that `as.numeric()` can read `x`; `sep_out` goes in at the
  # end:
  if (sep_in != ".") {
    x <- sub(sep_in, ".", x, fixed = TRUE)
  }

  # A value that is not a number becomes missing rather than being padded into
  # nonsense such as `"5%000"`. The strings `"NA"` and `""` are missing, not
  # non-numbers, and so is `NaN` in any spelling that `as.numeric()` reads, such as `"nan"`:
  x_num <- suppressWarnings(as.numeric(x))
  not_number <- is.na(x_num) & !is.nan(x_num) & !(x %in% c(NA, STRINGS_MISSING))
  if (any(not_number)) {
    cli::cli_warn(c(
      "Values that are not numbers became `NA`.",
      "x" = "This concerns {wrap_in_backticks(unique(x[not_number]))}."
    ))
  }
  x[is.na(x_num)] <- NA_character_
  x_orig <- x

  # Zeros appended to scientific notation such as `"1e-04"` would multiply the
  # value, so it is written out in full, to the decimal places that
  # `decimal_places()` reads off the exponent. So are other spellings that
  # `as.numeric()` accepts, such as `".5"`, `"+1.5"`, or `"0x1A"`:
  # Decimal spellings are rewritten as text, since a double holds only about
  # 17 significant digits and `sprintf()` would print binary noise beyond them;
  # only the rest, such as hexadecimal, goes through the number:
  rewrite <- is.finite(x_num) & !grepl("^-?[0-9]+(\\.[0-9]*)?$", x)
  as_text <- rewrite & grepl(PATTERN_DECIMAL_SPELLING, x)
  x[as_text] <- write_out_decimal(x[as_text])
  too_long <- as_text & is.na(x)
  if (any(too_long)) {
    cli::cli_warn(c(
      "Values with an exponent too large to write out became `NA`.",
      "x" = "This concerns {wrap_in_backticks(x_orig[too_long])}.",
      "i" = "Written out, their integer or decimal part would have more \\
      than {WRITTEN_OUT_MAX} digits."
    ))
  }
  as_number <- rewrite & !as_text
  # Not to the decimal places of the spelling: `"0x1p-2"` has none, but it is
  # 0.25.
  x[as_number] <- number_to_string(x_num[as_number])

  # `NA` for a missing value and an infinity:
  width_mantissa <- decimal_places(x)

  # Compared to the decimal counts as the whole number it is shown as:
  width <- round_whole(width)

  if (is.null(width)) {
    if (length(x) == 1L) {
      cli::cli_warn(
        c(
          "No trailing zeros can be restored",
          "!" = "`x` has length 1",
          ">" = "Specify `width` to predetermine a number of decimal places \\
          to which `x` values should be padded."
        ),
        class = "scrutiny_warning_no_zeros"
      )
    } else if (all(width_mantissa == 0L, na.rm = TRUE)) {
      cli::cli_warn(
        c(
          "No trailing zeros can be restored",
          "!" = "None of the {length(x)} `x` values has any decimal places.",
          ">" = "Specify `width` to predetermine a number of decimal places \\
          to which `x` values should be padded."
        ),
        class = "scrutiny_warning_no_zeros"
      )
    }
    width_target <- max(0L, width_mantissa, na.rm = TRUE)
  } else if (
    !is.numeric(width) ||
      !any(c(1L, length(x)) == length(width)) ||
      !all(is.finite(width)) ||
      !all(is_whole_number(width)) ||
      any(width < 0 | width > WRITTEN_OUT_MAX)
  ) {
    abort_in_export(
      "`width` must be a single whole number from 0 to {WRITTEN_OUT_MAX} \
      (or a vector of such numbers with the same length as `x`).",
      "x" = "It is {width}."
    )
  } else if (
    check_width == "capped" && any(width_mantissa > width, na.rm = TRUE)
  ) {
    # `width` may be one value per `x`, so the offenders are reported with
    # their own widths:
    which_wrong <- which(width_mantissa > width)
    offenders <- x[which_wrong]
    width_wrong <- rep_len(width, length(x))[which_wrong]
    abort_in_export(
      "Some values have more decimal places than `width` foresees.",
      "x" = "`width` was set to {unique(width_wrong)}.",
      "i" = "Values with more decimal places than that: {offenders}",
      ">" = "Avoid this error with `check_width = \"never\"`."
    )
  } else {
    width_target <- width
  }

  # Zeros are appended literally: `sprintf("%.*f")` would print the binary
  # expansion, so `0.1` padded to 20 places would be `"0.10000000000000000555"`:
  n_zeros <- width_target - width_mantissa
  pad <- !is.na(n_zeros) & n_zeros > 0L
  point <- dplyr::if_else(grepl(".", x[pad], fixed = TRUE), "", ".")
  out <- x
  out[pad] <- paste0(x[pad], point, strrep("0", n_zeros[pad]))

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
  check_width = c("capped", "never")
) {
  check_flag(check_numeric_like)
  check_flag(check_decimals)
  data <- as_data_frame_input(data, "`restore_zeros()`, without `_df`")

  # The function works on whole columns, and grouping would get in the way:
  # `dplyr::select()` re-adds the grouping columns, and `across()` runs per
  # group. The output is ungrouped:
  data <- dplyr::ungroup(data)

  # A column with comma decimals is only numeric-like when read with `sep_in`:
  sep_in_literal <- sep_literal(sep_in)
  names_num_cols <- data |>
    dplyr::select(where(function(x) is_numeric_like_col(x, sep_in_literal))) |>
    colnames()

  if (check_numeric_like) {
    selection2 <- rlang::expr(all_of(names_num_cols))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  if (check_decimals) {
    selection3 <- rlang::expr(where(function(x) {
      !is_numeric_like_col(x, sep_in_literal) ||
        any(decimal_places(x, sep = sep_in) > 0L, na.rm = TRUE)
    }))
  } else {
    # `everything()` is the identity element of `&` in tidyselect:
    selection3 <- rlang::expr(dplyr::everything())
  }

  cols_to_select <- rlang::expr({{ cols }} & !!selection2 & !!selection3)
  # A rename in `cols`, as in `c(new = a)`, would add a column:
  cols_to_select <- tidyselect::eval_select(
    cols_to_select,
    data,
    allow_rename = FALSE
  )

  # Columns that `cols` selected but that aren't numeric-like: skipped under
  # `check_numeric_like = TRUE`, else their non-numeric values become `NA`.
  # Either way, only columns named by `cols` are worth a warning: under the
  # default, a study-name column next to the numbers is nothing to report.
  names_selected <- if (missing(cols)) {
    character()
  } else {
    names(tidyselect::eval_select(
      rlang::expr({{ cols }}),
      data,
      allow_rename = FALSE
    ))
  }
  names_wrong_cols <- names_selected[!names_selected %in% names_num_cols]

  if (length(names_wrong_cols) > 0L) {
    msg_exclusion <- if (check_numeric_like) {
      paste0(c("was", "were"), " excluded")
    } else {
      c(
        "had its non-numeric values replaced by `NA`",
        "had their non-numeric values replaced by `NA`"
      )
    }
    warn_wrong_columns_selected(
      names_wrong_cols,
      msg_exclusion,
      msg_reason = "numeric-like",
      msg_it_they = c("It isn't", "They aren't")
    )
  }

  # A column of whole numbers, such as `n`, or a single row is normal in a data
  # frame, so `restore_zeros()`'s warning that there is nothing to pad would
  # only be noise here, and it would name the column `x`:
  # `dplyr::mutate()` would re-signal the warning on its own, so it is muffled
  # in the function that `across()` applies.
  #
  # That function is defined out here: dplyr inlines one written inside
  # `across()`, which then looks up `width` and the like among the columns of
  # `data` first, so that a column of such a name would replace the argument.
  restore <- function(x) {
    withCallingHandlers(
      restore_zeros(
        x = x,
        width = width,
        sep_in = sep_in,
        sep_out = sep_out,
        check_width = check_width
      ),
      scrutiny_warning_no_zeros = function(cnd) rlang::cnd_muffle(cnd)
    )
  }

  data |>
    dplyr::mutate(
      dplyr::across(
        .cols = all_of(cols_to_select),
        .fns = restore
      )
    )
}


# Writes such a number out without an exponent by moving the decimal point
# within the digits, so that every digit given is kept exactly. `NA` if the
# integer or decimal part would have more than `WRITTEN_OUT_MAX` digits, as for
# `"1e-999999"`. The
# exponent is read as a double, which can't overflow the way an integer can:
write_out_decimal <- function(x) {
  parts <- regmatches(x, regexec(PATTERN_DECIMAL_SPELLING, x))
  vapply(
    parts,
    function(part) {
      sign <- if (part[2L] == "-") "-" else ""
      digits <- paste0(part[3L], part[4L])
      exponent <- if (part[5L] == "") 0 else as.numeric(part[5L])
      point <- nchar(part[3L]) + exponent
      if (max(point, nchar(digits) - point) > WRITTEN_OUT_MAX) {
        return(NA_character_)
      }
      if (point <= 0L) {
        digits <- paste0(strrep("0", 1L - point), digits)
        point <- 1L
      } else if (point > nchar(digits)) {
        digits <- paste0(digits, strrep("0", point - nchar(digits)))
      }
      int <- sub("^0+(?=[0-9])", "", substr(digits, 1L, point), perl = TRUE)
      dec <- substr(digits, point + 1L, nchar(digits))
      paste0(sign, int, if (nchar(dec) > 0L) ".", dec)
    },
    character(1L),
    USE.NAMES = FALSE
  )
}
