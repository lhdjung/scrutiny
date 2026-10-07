#' Test whether a vector is numeric or coercible to numeric
#'
#' @description `is_numeric_like()` tests whether an object contains numbers, by
#'   scrutiny's standards:
#'
#'   - Integer and double vectors are `TRUE`.
#'   - Logical vectors and non-vectors are `FALSE`.
#'   - Other vectors, such as strings, are `TRUE` if every non-`NA` value can
#'   be coerced to a non-`NA` number.
#'   - Factors are tested as strings.
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
  # `unlist()` would silently turn `TRUE` into `1` next to a number, and a
  # factor into its codes. So a logical element is `FALSE`, as a logical vector
  # is, and a factor is read as its label:
  if (is.list(x) && length(x) > 0L) {
    is_logical_value <- function(e) is.logical(e) && !is.na(e)
    if (any(vapply(x, is_logical_value, logical(1L), USE.NAMES = FALSE))) {
      return(FALSE)
    }
    x <- lapply(x, function(e) if (is.factor(e)) as.character(e) else e)
    return(is_numeric_like(unlist(x, recursive = FALSE, use.names = FALSE)))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  x <- suppressWarnings(as.numeric(x))
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
