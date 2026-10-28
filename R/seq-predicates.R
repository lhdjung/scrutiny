# Helpers for the sequence predicates below:

is_seq_ascending_basic <- function(x) {
  all(diff(x) > 0)
}


is_seq_descending_basic <- function(x) {
  all(diff(x) < 0)
}


# Non-exported workhorse API of all the sequence predicates:
is_seq_basic <- function(
  x,
  tolerance = .Machine$double.eps^0.5,
  test_linear = TRUE,
  test_special = NULL,
  min_length = NULL,
  from = NULL
) {
  if (!is.null(test_special) && test_special == "dispersed") {
    # Forced first so that a missing `from` errors rather than being skipped by
    # an early `FALSE`:
    force(from)

    # A dispersed sequence has one central value:
    if (is_even(length(x))) {
      return(FALSE)
    }

    if (!is_numeric_like(from)) {
      return(FALSE)
    }

    # A factor by its label, not its level code, as `x` below:
    if (is.factor(from)) {
      from <- as.character(from)
    }
    from <- as.numeric(from)
  }

  # Too short for the sequence in question, whatever the values:
  if (!is.null(min_length) && length(x) < min_length) {
    return(FALSE)
  }

  # A vector of all `NA`s leaves the question open, whatever its type.
  # `is_numeric_like()` is `FALSE` for a logical vector, which includes `c(NA,
  # NA, NA)`, and `NA` for a character one, which the `if` below can't take:
  if (length(x) > 0L && all(is.na(x))) {
    return(NA)
  }

  if (isFALSE(is_numeric_like(x))) {
    return(FALSE)
  }

  # Everything below is arithmetic on `x`. A factor is read by its labels, not
  # its integer codes:
  if (!is.numeric(x)) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    x <- as.numeric(x)
  }

  if (length(x) == 1L) {
    return(TRUE)
  }

  if (anyNA(x)) {
    x_orig <- x

    not_na <- which(!is.na(x))

    # Leading and trailing `NA`s cannot disprove that `x` is the kind of
    # sequence in question, only keep it from being proven:
    x <- x[not_na[1L]:not_na[length(not_na)]]

    # As in `vignette("devtools")`: are the known values consistent with each
    # other, given their positions? For linearity, every pair of neighboring
    # known values must imply the same step per index, so `c(1, NA, 5, 7)` can
    # be linear:
    known <- which(!is.na(x))

    if (test_linear) {
      steps <- diff(x[known]) / diff(known)
      if (!isTRUE(all(dplyr::near(steps, steps[1L], tol = tolerance)))) {
        return(FALSE)
      }
    }

    # The special tests, too, ask whether the known values already rule the
    # sequence out. Monotonicity is disproven by the known values alone. A
    # dispersed sequence has each mirrored pair of values centered on `from`,
    # which a fully known pair can disprove; the central value is its own
    # mirror image:
    if (!is.null(test_special)) {
      pass_test_special <- switch(
        test_special,
        "ascending" = is_seq_ascending_basic(x[known]),
        "descending" = is_seq_descending_basic(x[known]),
        "dispersed" = {
          centers <- (x_orig + rev(x_orig)) / 2
          all(dplyr::near(centers[!is.na(centers)], from, tol = tolerance))
        }
      )
      if (!isTRUE(pass_test_special)) {
        return(FALSE)
      }
    }

    return(NA)
  }

  # Steps are compared with their signs, so a zigzag like `c(1, 2, 1)` is not
  # linear:
  if (test_linear) {
    # An infinity makes a step `Inf - Inf`, which is `NaN` and equal to no
    # other step, hence `isTRUE()`, here and in the other tests:
    steps <- diff(x)
    if (!isTRUE(all(dplyr::near(steps, steps[1L], tol = tolerance)))) {
      return(FALSE)
    }
  }

  if (!is.null(test_special)) {
    pass_test_special <- switch(
      test_special,
      "ascending" = is_seq_ascending_basic(x),
      "descending" = is_seq_descending_basic(x),
      "dispersed" = is_seq_dispersed_basic(x, from, tolerance)
    )
    if (!isTRUE(pass_test_special)) {
      return(FALSE)
    }
  }

  TRUE
}


#' Is a vector a certain kind of sequence?
#'
#' @description Predicates for numeric vectors, or vectors coercible to
#'   numeric:
#'
#'   - `is_seq_linear()`: do all consecutive elements differ by the same
#'   amount?
#'   - `is_seq_ascending()`, `is_seq_descending()`: is every difference
#'   positive, or negative?
#'   - `is_seq_dispersed()`: is `x` centered on `from`, with each pair of
#'   values equidistant from it?
#'
#'   By default (`test_linear = TRUE`), the last three also test for linearity.
#'
#' @param x Numeric, or coercible to numeric as determined by
#'   [`is_numeric_like()`].
#' @param from Numeric or coercible to numeric. Only in `is_seq_dispersed()`.
#'   The value `x` should be centered on.
#' @param test_linear Logical. Should linearity be tested as well? Default is
#'   `TRUE`. Not in `is_seq_linear()`.
#' @param tolerance Numeric. Tolerance for comparing values. Default is about
#'   `1.5e-8`, as in [`dplyr::near()`].
#'
#' @return Logical (length 1). If `x` contains `NA`:
#'   - `NA` if all elements are `NA`, unless `x` is too short for the kind of
#'   sequence in question: two values for `is_seq_ascending()` and
#'   `is_seq_descending()`, three for `is_seq_dispersed()`.
#'   - Otherwise, `NA` if some values substituted for the `NA`s would make `x`
#'   the kind of sequence in question, and `FALSE` if none would.
#'
#'   A missing `from` is handled the same way.
#'
#'   See `vignette("devtools")`, section *NA handling*.
#'
#' @seealso `validate::is_linear_sequence()`, which is more permissive with
#'   `NA` and supports date-times.
#'
#' @export
#'
#' @name seq-predicates
#'
#' @examples
#' is_seq_linear(x = 3:7)
#' is_seq_linear(x = c(3:7, 9))
#'
#' # The other predicates test for linearity by default:
#' is_seq_ascending(x = c(2, 7, 9))
#' is_seq_ascending(x = c(2, 7, 9), test_linear = FALSE)
#'
#' is_seq_descending(x = c(9, 7, 2))
#' is_seq_descending(x = c(9, 7, 2), test_linear = FALSE)
#'
#' is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5)
#' is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5, test_linear = FALSE)
#'
#' # These fail their own test regardless of linearity:
#' is_seq_ascending(x = c(1, 7, 4), test_linear = FALSE)
#' is_seq_descending(x = c(9, 15, 3), test_linear = FALSE)
#' is_seq_dispersed(1:10, from = 5, test_linear = FALSE)

is_seq_linear <- function(x, tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(x, tolerance, test_linear = TRUE)
}


#' @rdname seq-predicates
#' @export

is_seq_ascending <- function(
  x,
  test_linear = TRUE,
  tolerance = .Machine$double.eps^0.5
) {
  is_seq_basic(
    x,
    tolerance,
    test_linear,
    test_special = "ascending",
    min_length = 2L
  )
}


#' @rdname seq-predicates
#' @export

is_seq_descending <- function(
  x,
  test_linear = TRUE,
  tolerance = .Machine$double.eps^0.5
) {
  is_seq_basic(
    x,
    tolerance,
    test_linear,
    test_special = "descending",
    min_length = 2L
  )
}


#' @rdname seq-predicates
#' @export

is_seq_dispersed <- function(
  x,
  from,
  test_linear = TRUE,
  tolerance = .Machine$double.eps^0.5
) {
  check_length(from, 1L)

  # A missing center leaves the question open unless `x` rules out every
  # center. A dispersed sequence is centered on each mirrored pair of its
  # values, so the first known such center is the only candidate:
  if (is.na(from)) {
    x_num <- if (is.factor(x)) as.character(x) else x
    x_num <- suppressWarnings(as.numeric(x_num))
    centers <- (x_num + rev(x_num)) / 2
    candidate <- centers[!is.na(centers)][1L]
    out <- if (is.na(candidate)) {
      !is_even(length(x)) &&
        !isFALSE(is_seq_basic(x, tolerance, test_linear, min_length = 3L))
    } else {
      is_seq_dispersed(x, candidate, test_linear, tolerance)
    }
    return(if (isFALSE(out)) FALSE else NA)
  }
  is_seq_basic(
    x,
    tolerance,
    test_linear,
    test_special = "dispersed",
    min_length = 3L,
    from = from
  )
}


# Helper, not exported:
is_seq_dispersed_basic <- function(
  x,
  from,
  tolerance = .Machine$double.eps^0.5
) {
  if (is_even(length(x))) {
    return(FALSE)
  }

  x <- as.numeric(x)
  from <- as.numeric(from)

  index_central_x <- index_central(x)

  if (!dplyr::near(x[index_central_x], from, tolerance)) {
    return(FALSE)
  }

  dispersion_minus <- from - x[1L:(index_central_x - 1L)]
  dispersion_plus <- from + x[(index_central_x + 1L):length(x)]

  from_reconstructed <- (dispersion_plus - rev(dispersion_minus)) / 2

  all(dplyr::near(from, from_reconstructed, tolerance))
}
