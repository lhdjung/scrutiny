#' Test whether a vector is numeric or coercible to numeric
#'
#' @description `is_numeric_like()` tests whether an object contains numbers, by
#'   scrutiny's standards:
#'
#'   - Integer and double vectors are `TRUE`.
#'   - Logical vectors and non-vectors are `FALSE`.
#'   - Other vectors, such as strings, are `TRUE` if every non-`NA` value can
#'   be coerced to a non-`NA` number. The strings `"NA"` and `"NaN"` count as
#'   `NA`, and so does a blank string, as in a blank cell of a CSV file.
#'   - Factors, dates, and times are tested as the strings they print as.
#'   - Lists are tested like atomic vectors if every element has length 1, and
#'   are `FALSE` otherwise. A logical element other than `NA` makes a list
#'   `FALSE`.
#'   - A vector of nothing but non-numeric, non-logical `NA` is `NA`.
#'
#' @param x Object to be tested.
#'
#' @details scrutiny often works with number-strings, since only strings (and
#'   factors) preserve trailing zeros. This function asks whether a vector
#'   displays numbers, not how it is stored. Logical vectors are `FALSE` because
#'   they display as `TRUE` and `FALSE`, and factors are tested as strings
#'   because that is what they display.
#'
#' @return Logical (length 1).
#'
#' @export
#'
#' @examples
#' is_numeric_like(x = 1:5)
#' is_numeric_like(x = c(TRUE, FALSE))
#'
#' is_numeric_like(x = c("42", "0.7", NA))
#' is_numeric_like(x = c("42", "xyz", NA))
#'
#' is_numeric_like(x = as.factor(c("42", "0.7", NA)))
#'
#' is_numeric_like(x = list("42", "0.7", NA))
#' is_numeric_like(x = list("42", "0.7", NA, c(1, 2, 3)))
#'
#' # All `NA`:
#' is_numeric_like(x = as.character(c(NA, NA, NA)))
#' is_numeric_like(x = as.numeric(c(NA, NA, NA)))
#' is_numeric_like(x = c(NA, NA, NA))

is_numeric_like <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  if (
    is.logical(x) ||
      !rlang::is_vector(x) ||
      is.list(x) &&
        !all(vapply(
          x,
          function(x) length(x) == 1L,
          logical(1L),
          USE.NAMES = FALSE
        ))
  ) {
    return(FALSE)
  }
  # A list is tested like the atomic vector of its elements -- except that
  # `unlist()` would silently turn `TRUE` into `1` next to a number, a factor
  # into its codes, and a date into a count of days. So a logical element is
  # `FALSE`, as a logical vector is, and anything else is read as it prints:
  if (is.list(x) && length(x) > 0L) {
    is_logical_value <- function(e) is.logical(e) && !is.na(e)
    if (any(vapply(x, is_logical_value, logical(1L), USE.NAMES = FALSE))) {
      return(FALSE)
    }
    # Each element on its own: `unlist()` would write a number next to a
    # string with the decimal mark of `options(OutDec = )`.
    # A plain `NA` is logical, but missing all the same:
    is_empty <- function(e) is.null(e) || (is.logical(e) && all(is.na(e)))
    out <- x[!vapply(x, is_empty, logical(1L), USE.NAMES = FALSE)] |>
      vapply(is_numeric_like, logical(1L), USE.NAMES = FALSE)
    # Nothing but plain `NA`s is a logical vector, which is not numeric-like:
    if (length(out) == 0L || any(!out, na.rm = TRUE)) {
      return(FALSE)
    }
    return(if (all(is.na(out))) NA else TRUE)
  }
  # A factor is read as its labels, and a date or time as the string it prints
  # as, not as the number of days or seconds it is stored as:
  if (!is.character(x)) {
    x <- as.character(x)
  }
  # `as.numeric()` only trims ASCII whitespace, but `decimal_places()` also
  # reads a value next to a non-breaking space, as from a copied table:
  x <- trimws(x[!is.na(x)], whitespace = "[\\h\\v]")
  # The strings `"NA"` and `""` are missing, as in `decimal_places()` and
  # `restore_zeros()`, which also trim them first:
  x <- x[!x %in% STRINGS_MISSING]
  x <- suppressWarnings(as.numeric(x))
  # So is the string `"NaN"`, as `NaN` is everywhere in scrutiny:
  x <- x[!is.nan(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  !anyNA(x)
}


# `is_numeric_like()` for column selection in the `*_df()` functions. A column
# of nothing but missing values is `NA` there, which `where()` can't take;
# nothing in it contradicts a number, so it counts as numeric-like. A literal
# decimal separator other than a point is read as one.
is_numeric_like_col <- function(x, sep = ".") {
  if (sep != "." && (is.character(x) || is.factor(x))) {
    x <- sub(sep, ".", as.character(x), fixed = TRUE)
  }
  !isFALSE(is_numeric_like(x))
}
