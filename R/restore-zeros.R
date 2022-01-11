

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
#' @details This function exploits the fact that groups of summary values such
#'   as means or percentages are often reported to the same number of decimal
#'   places. If such a number is known but values were not entered as strings,
#'   trailing zeros will be lost. In this case, `restore_zeros()` will be
#'   helpful to prepare data for consistency testing functions such as
#'   `grim_map()` or `debit_map()`. Otherwise, it should probably not be used.
#'
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
#' @param sep Substring that separates the mantissa from the integer part.
#'   Default is `"\\."`, which renders a decimal point.
#'
#' @return A string vector. At least some of the strings will have newly
#'   restored zeros, unless all input values had the same number of decimal
#'   places.
#'
#' @export
#'
#' @include utils.R
#'
#' @seealso `decimal_places()`
#'
#' @examples
#' # By default, the target width is that of
#' # the longest mantissa:
#' vec <- c(212, 75.38, 4.9625)
#' vec %>% restore_zeros()
#'
#' # Alternatively, supply a number via `width`:
#' vec %>% restore_zeros(width = 6)


restore_zeros <- function(x, width = NULL, sep = "\\.") {

  # # To get trailing zeros, all elements need to be strings:
  # x <- as.character(x)

  # Make sure no whitespace (from values that already were strings) is factored
  # into the count:
  x <- stringr::str_trim(x)

  # Count characters of the integer and mantissa parts via internal helper
  # functions:
  # integer_length <- integer_length(x)
  # mantissa_length <- mantissa_length(x)

  parts <- stringr::str_split_fixed(x, sep, n = 2)
  integer_length <- stringr::str_length(parts[, 1])
  mantissa_length <- stringr::str_length(parts[, 2])

  # Determine the maximal width to which the mantissas should be padded in
  # accordance with the `width` argument, the default of which, `NULL`, makes
  # the function go for the maximal length of already-present mantissas:
  if (is.null(width)) {
    if (length(x) == 1) {
      rlang::warn(glue::glue(
        "If `x` has length 1, trailing zeros can't be restored without \\
        specifying `width`."
      ))
    } else if (purrr::every(mantissa_length, `==`, 0)) {
      rlang::warn(glue::glue(
        "None of the {length(x)} `x` values has any decimal places, so no \\
        zeros can be restored without specifying `width`."
      ))
    }
    target_width <- max(mantissa_length, na.rm = TRUE)
  } else {
    target_width <- width
  }

  # The number of trailing zeros to be added takes the respective lengths of
  # three variables into account: the integer part (via an internal helper
  # function), the decimal point (i.e., 1), and the target mantissa length
  # (determined above). Note that the integer part is only relevant for being
  # balanced against, precisely so that it ultimately plays no role. Also note
  # that `"i"` and `"d"` are just placeholders here, used to make sure there is
  # exactly one `sep` substring (default is a decimal point) in each resulting
  # string value...
  out <- stringr::str_pad(x, width = integer_length + 1 + target_width,
                          side = "right", pad = "i") %>%
    stringr::str_replace(    "i", sep) %>%
    stringr::str_replace_all("i", "0") %>%
    stringr::str_replace(    sep, "d") %>%
    stringr::str_replace_all(sep, "0") %>%
    stringr::str_replace(    "d", sep)

  # ...unless a string has no decimal places, in which case the `sep` substring
  # at the end of the value would be superfluous. It is removed before the
  # output is returned:
  dplyr::if_else(
    stringr::str_detect(out, paste0(sep, "$")),  # used to have: "\\.$"
    stringr::str_remove(out, sep),
    out
  )

}


