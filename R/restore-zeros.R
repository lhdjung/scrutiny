

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

  # Make sure no whitespace (from values that already were strings) is factored
  # into the count:
  x <- stringr::str_trim(x)

  # Count characters of the mantissa part:
  parts <- stringr::str_split_fixed(x, sep, n = 2)
  width_mantissa <- stringr::str_length(parts[, 2])

  # Determine the maximal width to which the mantissas should be padded in
  # accordance with the `width` argument, the default of which, `NULL`, makes
  # the function go by the maximal length of already-present mantissas:
  if (is.null(width)) {
    if (length(x) == 1) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "`x` has length 1",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    } else if (purrr::every(width_mantissa, `==`, 0)) {
      cli::cli_warn(c(
        "No trailing zeros can be restored",
        "!" = "None of the {length(x)} `x` values has any decimal places.",
        ">" = "Specify `width` to predetermine a number of decimal places \\
        to which `x` values should be padded."
      ))
    }
    width_target <- max(width_mantissa, na.rm = TRUE)
  } else {
    width_target <- width
  }

  # Assemble the formatting regex, determined by `width_target` -- the desired
  # number of decimal places to which the `x` values should be padded:
  format <- paste0("%.", width_target, "f")

  # Pad `x` with the correct amount of trailing zeros:
  sprintf(format, as.numeric(x))

}


