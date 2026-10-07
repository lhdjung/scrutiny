# Single-case function; not exported but used as a basis for the vectorized
# `add_up_to()` as well as within `add_up_to_map()`:
add_up_to_scalar <- function(summands, total) {
  if (!is.numeric(summands) && !all(is.na(summands))) {
    check_type(summands, c("double", "integer"))
  }
  if (!is.numeric(total) && !is.na(total)) {
    check_type(total, c("double", "integer"))
  }

  values <- c(summands, total)

  # Counts are whole numbers, and a sum of whole numbers is exact, so `==` needs
  # no tolerance. A sum of decimals is not exact (`0.1 + 0.2 != 0.3`), and a
  # fractional count describes no group, so such a case is undecidable rather
  # than given a verdict that floating-point error could decide:
  if (
    !all(is.finite(values)) ||
      any(abs(values - round(values)) >= WHOLE_NUMBER_TOLERANCE)
  ) {
    return(NA)
  }

  sum(summands) == total
}


#' Do reported counts add up to their total?
#'
#' @description `add_up_to()` checks whether reported subgroup sizes, or other
#'   counts, sum to the reported total.
#'
#'   The function is vectorized over cases, but [`add_up_to_map()`] is more
#'   convenient for testing many cases.
#'
#' @param summands Integer. The counts of one case, or a list with one such
#'   vector per case.
#' @param total Integer. The reported total, one per case.
#'
#' @return Logical. `TRUE` if the counts add up to `total`, `FALSE` if not,
#'   and `NA` if a value is missing or not a whole number.
#'
#' @include utils.R
#'
#' @export
#'
#' @examples
#' add_up_to(c(40, 37, 43), total = 120)
#' add_up_to(c(40, 37, 44), total = 120)
#'
#' # Several cases:
#' add_up_to(list(c(40, 37, 43), c(10, 12)), total = c(120, 23))
add_up_to <- function(summands, total) {
  # A plain vector is one case; `vectorize_test()` maps over list elements:
  if (!is.list(summands)) {
    summands <- list(summands)
  }
  vectorize_test(
    fun = add_up_to_scalar,
    frame = environment(),
    along = c("summands", "total")
  )
}
