
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
#'   testing functions such as `grim_map()` or `grimmer_map()`.

#' @section Displaying decimal places: You might not see all decimal places of
#'   numeric values in a vector, and consequently wonder if `restore_zeros()`,
#'   when applied to the vector, adds too many zeros. That is because displayed
#'   numbers, unlike stored numbers, are often rounded.
#'
#'   For a vector `x`, you can count the characters of the longest mantissa from
#'   among its values like this:
#'
#'   `x %>% decimal_places() %>% max()`
#'
#' @param x Numeric (or string coercible to numeric). Vector of numbers that
#'   might have lost trailing zeros.
#' @param width Integer. Number of decimal places the mantissas should have,
#'   including the restored zeros. Default is `NULL`, in which case the number
#'   of characters in the longest mantissa will be used instead.
#' @param sep_in Substring that separates the input's mantissa from its integer
#'   part. Default is `"\\."`, which renders a decimal point.
#' @param sep_out Substring that will be returned in the output to separate the
#'   mantissa from the integer part. By default, `sep_out` is the same as
#'   `sep_in`.
#' @param sep [[Deprecated]] Use `sep_in`, not `sep`. If `sep` is specified
#'   nonetheless, `sep_in` takes on `sep`'s value.
#' @param data Data frame or matrix. Only in `restore_zeros_df()`, and instead
#'   of `x`.
#' @param cols Only in `restore_zeros_df()`. Select columns from `data` using
#'   \href{https://tidyselect.r-lib.org/reference/language.html}{tidyselect}.
#'   Default is `everything()`, which selects all columns that pass the test of
#'   `check_numeric_like`.
#' @param check_numeric_like Boolean. Only in `restore_zeros_df()`. If `TRUE`
#'   (the default), the function will skip columns that are not numeric or
#'   coercible to numeric, as determined by `is_numeric_like()`.
#' @param check_decimals Boolean. Only in `restore_zeros_df()`. If set to
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
#' @seealso Wrapped functions: `sprintf()` and `decimal_places()`.
#'
#' @examples
#' # By default, the target width is that of
#' # the longest mantissa:
#' vec <- c(212, 75.38, 4.9625)
#' vec %>%
#'   restore_zeros()
#'
#' # Alternatively, supply a number via `width`:
#' vec %>%
#'   restore_zeros(width = 6)
#'
#' # For better printing:
#' iris <- tibble::as_tibble(iris)
#'
#' # Apply `restore_zeros()` to all numeric
#' # columns, but not to the factor column:
#' iris %>%
#'   restore_zeros_df()
#'
#' # Select columns as in `dplyr::select()`:
#' iris %>%
#'   restore_zeros_df(starts_with("Sepal"), width = 3)


restore_zeros <- function(x, width = NULL, sep_in = "\\.", sep_out = sep_in,
                          sep = NULL) {

  # Make sure no whitespace (from values that already were strings) is factored
  # into the count:
  x <- stringr::str_trim(x)

  # The deprecated `sep` argument was replaced by `sep_in`. Therefore, if `sep`
  # is still specified...
  if (!is.null(sep)) {
    if (sep_in != "\\.") {
      cli::cli_abort(c(
        "!" = "`sep` is deprecated. It was replaced by `sep_in`.",
        "x" = "`sep` conflicts with `sep_in`",
        "i" = "If `sep` is still specified, `sep_in` takes on its value."
      ))
    } else {
      cli::cli_warn(c(
        "`sep` is deprecated",
        ">" = "Use `sep_in`, not `sep`."
      ))
    }

    # ... `sep_in` must take on its role:
    sep_in <- sep
  }

  # Determine the maximal width to which the mantissas should be padded in
  # accordance with the `width` argument, the default of which, `NULL`, makes
  # the function go by the maximal length of already-present mantissas:
  if (is.null(width)) {

    # Count characters of the mantissa part:
    parts <- stringr::str_split_fixed(x, sep_in, n = 2L)
    width_mantissa <- stringr::str_length(parts[, 2])

    # Throw a warning if `x` can't be formatted with the given arguments:
    if (length(x) == 1L) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "`x` has length 1",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    } else if (all(width_mantissa == 0L)) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "None of the {length(x)} `x` values has any decimal places.",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    }

    # The number of decimal places to which `x` values will be padded with zeros
    # is determined by the number of characters in the longest mantissa...
    width_target <- max(width_mantissa, na.rm = TRUE)

  } else {

    # ... unless the user manually specified that target number via `width`:
    width_target <- width
  }

  # In `x`, if integers and mantissas are separated by something other than
  # decimal points, these separators need to be temporarily changed to points,
  # so that `sprintf()` will be able to operate on `x` below:
  if (any(sep_in != "\\.")) {
    x <- stringr::str_replace(x, sep_in, "\\.")
  }

  # Assemble the formatting expression, determined by `width_target` -- the
  # desired number of decimal places to which the `x` values should be padded:
  out_format <- paste0("%.", width_target, "f")

  # Pad `x` with the correct amount of trailing zeros:
  out <- sprintf(out_format, as.numeric(x))

  # By default, the separator in the output vector should be a decimal point,
  # but it might have been overridden -- either directly via `sep_out` or
  # indirectly via `sep_in` (because the default for `sep_out` is `sep_in`). If
  # so, it now takes its place again. In any case, the output is returned:
  if (all(sep_out == "\\.")) {
    out
  } else {
    stringr::str_replace(out, "\\.", sep_out)
  }

}


#' @rdname restore_zeros
#' @export

restore_zeros_df <- function(data, cols = everything(),
                             check_numeric_like = TRUE, check_decimals = FALSE,
                             width = NULL, sep_in = "\\.", sep_out = NULL,
                             sep = NULL, ...) {

  # Check whether the user specified any "old" arguments: those starting on a
  # dot. This check is now the only remaining purpose of the `...` dots because
  # these are no longer meant to be used. Any other arguments passed through
  # them should still lead to an error:
  check_new_args_without_dots(
    data, dots = rlang::enquos(...), old_args = c(
      ".data", ".check_decimals", ".width", ".sep_in", ".sep_out", ".sep"
    ), name_fn = "restore_zeros_df"
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

  # Names of selection-suitable columns:
  names_num_cols <- data %>%
    dplyr::select(where(is_numeric_like)) %>%
    colnames()

  # By default, selection is restricted to columns that are numeric or coercible
  # to numeric. This is checked with an internal helper from the utils.R file:
  if (check_numeric_like) {
    selection2 <- rlang::expr(all_of(names_num_cols))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  # If desired by the user, create an additional selection criterion: In a
  # numeric-like column, at least one value must have at least one decimal
  # place. Otherwise...
  if (check_decimals) {
    selection3 <- rlang::expr(where(
      function(x) has_decimals_if_numeric_like(x, sep = sep_in)
    ))
  } else {
    # ... the new variable is set up to be evaluated as `everything()`, which is
    # an identity element of the `&` operator in tidyselect:
    selection3 <- rlang::expr(dplyr::everything())
  }

  # Column selection is outsourced here so that the resultv can also be used in
  # a check below:
  cols_to_select <- rlang::expr({{ cols }} & !!selection2 & !!selection3)
  cols_to_select <- tidyselect::eval_select(cols_to_select, data)

  # Check whether any selected columns are not numeric-like, in which case they
  # can't have any decimal places restored. If so...
  names_cols_select <- names(cols_to_select)
  names_wrong_cols <- names_cols_select[!names_cols_select %in% names_num_cols]

  # ...the user is warned:
  if (length(names_wrong_cols) > 0L) {
    warn_wrong_columns_selected(
      names_wrong_cols,
      msg_exclusion = "didn't have any decimal places restored",
      msg_reason = "numeric-like",
      msg_it_they = c("It isn't", "They aren't")
    )
  }

  # Save memory by removing objects that are no longer needed:
  rm(names_num_cols, selection2, selection3)

  # By default, a columns is selected if and only if it's numeric-like.
  # Additional constrains might come via `selection2` or `selection3` (see
  # `cols_to_select` above; by default, only `selection2` takes effect). The
  # `.fns` argument uses an anonymous function to pass on all the named
  # arguments to `restore_zeros()`:
  dplyr::mutate(data, dplyr::across(
    .cols = all_of(cols_to_select),
    .fns = function(data_dummy) {
      restore_zeros(
        x = data_dummy, width = width,
        sep_in = sep_in, sep_out = sep_out, sep = sep
      )
    }
  ))

}

