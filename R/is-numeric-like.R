
#' Test whether a vector is numeric or coercible to numeric
#'
#' @description `is_numeric_like()` tests whether an object is "coercible to
#'   numeric" by the standards of scrutiny. This means:
#'
#'   - Integer and double vectors are `TRUE`.
#'   - Booleans are always `FALSE`, as are non-vector objects.
#'   - Other vectors (most likely strings) are `TRUE` if all their non-`NA`
#'   values can be coerced to non-`NA` numeric values, and `FALSE` otherwise.
#'   - Factors are first coerced to string, then tested.
#'   - If all values are `NA`, the output is also `NA`.
#'
#'   See details for discussion.
#'
#' @param x Object to be tested.
#'
#' @details The scrutiny package often deals with "number-strings", i.e.,
#'   strings that can be seamlessly coerced to numeric. This is a matter of
#'   representing numbers in a certain format, so what counts here is the way
#'   values are displayed, not how they are stored.
#'
#'   `is_numeric_like()` returns `FALSE` for Booleans because they are displayed
#'   as words, not as numbers, and the usual coercion rules would be misleading
#'   in this context. Likewise, the function treats factors like strings because
#'   that is much closer to the way they are displayed -- the fact that factors
#'   are stored as integers is irrelevant.
#'
#' @return Boolean (length 1).
#'
#' @seealso The \href{https://vctrs.r-lib.org/}{vctrs} package provides a
#'   serious typing framework for R; much in contrast to this rather ad-hoc and
#'   use case-specific function.
#'
#' @export
#'
#' @examples
#' # Numeric vectors are `TRUE`:
#' is_numeric_like(1:5)
#' is_numeric_like(2.47)
#'
#' # Strings are `TRUE` if they can be coerced
#' # to non-`NA` numbers, and `FALSE` otherwise:
#' is_numeric_like("42")
#' is_numeric_like("xyz")
#'
#' # Factors are treated like their
#' # string equivalents:
#' is_numeric_like(as.factor("42"))
#' is_numeric_like(as.factor("xyz"))
#'
#' # Booleans are always false:
#' is_numeric_like(c(TRUE, FALSE))
#'
#' # If all values are `NA`, so is the output:
#' is_numeric_like(c(NA, NA, NA))


is_numeric_like <- function(x) {
  if (is.logical(x)) {
    if (all(is.na(x))) {
      return(NA)
    } else {
      return(FALSE)
    }
  }
  if (!rlang::is_vector(x)) {
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

