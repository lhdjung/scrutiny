
#' Test whether a vector is numeric or coercible to numeric
#'
#' @description `is_numeric_like()` tests whether an object is "coercible to
#'   numeric" by the particular standards of scrutiny. This means:
#'
#'   - Integer and double vectors are `TRUE`.
#'   - Logical vectors are `FALSE`, as are non-vector objects.
#'   - Other vectors (most likely strings) are `TRUE` if all their non-`NA`
#'   values can be coerced to non-`NA` numeric values, and `FALSE` otherwise.
#'   - Factors are first coerced to string, then tested.
#'   - Lists are tested like atomic vectors unless any of their elements have
#'   length greater 1, in which case they are always `FALSE`.
#'   - If all values are non-numeric, non-logical `NA`, the output is also `NA`.
#'
#'   See details for discussion.
#'
#' @param x Object to be tested.
#'
#' @details The scrutiny package often deals with "number-strings", i.e.,
#'   strings that can be coerced to numeric without introducing new `NA`s. This
#'   is a matter of displaying data in a certain way, as opposed to their
#'   storage mode.
#'
#'   `is_numeric_like()` returns `FALSE` for logical vectors simply because
#'   these are displayed as strings, not as numbers, and the usual coercion
#'   rules would be misleading in this context. Likewise, the function treats
#'   factors like strings because that is how they are displayed: the fact that
#'   factors are stored as integers is irrelevant.
#'
#'   Why store numbers as strings or factors? Only these data types can preserve
#'   trailing zeros, and only if the data were originally entered as strings.
#'   See `vignette("wrangling")`, section *Trailing zeros*.
#'
#' @return Logical (length 1).
#'
#' @seealso The \href{https://vctrs.r-lib.org/}{vctrs} package, which provides a
#'   serious typing framework for R; in contrast to this rather ad-hoc function.
#'
#' @export
#'
#' @examples
#' # Numeric vectors are `TRUE`:
#' is_numeric_like(x = 1:5)
#' is_numeric_like(x = 2.47)
#'
#' # Logical vectors are always `FALSE`:
#' is_numeric_like(x = c(TRUE, FALSE))
#'
#' # Strings are `TRUE` if all of their non-`NA`
#' # values can be coerced to non-`NA` numbers,
#' # and `FALSE` otherwise:
#' is_numeric_like(x = c("42", "0.7", NA))
#' is_numeric_like(x = c("42", "xyz", NA))
#'
#' # Factors are treated like their
#' # string equivalents:
#' is_numeric_like(x = as.factor(c("42", "0.7", NA)))
#' is_numeric_like(x = as.factor(c("42", "xyz", NA)))
#'
#' # Lists behave like atomic vectors if all of their
#' # elements have length 1...
#' is_numeric_like(x = list("42", "0.7", NA))
#' is_numeric_like(x = list("42", "xyz", NA))
#'
#' # ...but if they don't, they are `FALSE`:
#' is_numeric_like(x = list("42", "0.7", NA, c(1, 2, 3)))
#'
#' # If all values are `NA`, so is the output...
#' is_numeric_like(x = as.character(c(NA, NA, NA)))
#'
#' # ...unless the `NA`s are numeric or logical:
#' is_numeric_like(x = as.numeric(c(NA, NA, NA)))
#' is_numeric_like(x = c(NA, NA, NA))


is_numeric_like <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  if (
    is.logical(x) ||
    !rlang::is_vector(x) ||
    is.list(x) && !all(vapply(
      x, function(x) length(x) == 1L, logical(1L), USE.NAMES = FALSE
    ))
  ) {
    return(FALSE)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  x <- suppressWarnings(as.numeric(x))
  !any(is.na(x))
}

