# Helpers for `function_map_seq()` as well as its assorted `reverse_*()` and
# `summarize_*()` functions:

is_seq_linear_basic <- function(x) {
  if (length(x) < 3L) {
    return(TRUE)
  }
  # Every successive pair of values must differ by the same amount, so each
  # pairwise difference must equal the first one.
  steps <- diff(x)
  all(steps == steps[1L])
}

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
    # Without the `force()` call, the function may return `FALSE` early, even if
    # `from` was not supplied:
    force(from)

    # A dispersed sequence requires one central value, so the number of elements
    # in `x` must be odd:
    if (is_even(length(x))) {
      return(FALSE)
    }

    # `from` is compared to values of `x` below, so it has to be a number, as
    # `x` is made one right after this:
    if (!is_numeric_like(from)) {
      return(FALSE)
    }
    from <- as.numeric(from)
  }

  # A vector of all `NA`s leaves the question open, whatever its type.
  # `is_numeric_like()` is `FALSE` for a logical vector, which includes `c(NA,
  # NA, NA)`, and `NA` for a character one, which the `if` below can't take:
  if (length(x) > 0L && all(is.na(x))) {
    return(NA)
  }

  if (!is_numeric_like(x)) {
    return(FALSE)
  }

  # Everything below is arithmetic on `x`. A string vector is admitted by the
  # check above but failed in `diff()`, and a factor was silently tested by its
  # integer codes, so `factor(c(1, 2, 4))` was a linear sequence:
  if (!is.numeric(x)) {
    if (is.factor(x)) {
      x <- as.character(x)
    }
    x <- as.numeric(x)
  }

  if (!is.null(min_length) && length(x) < min_length) {
    return(FALSE)
  }

  if (length(x) == 1L) {
    return(TRUE)
  }

  if (anyNA(x)) {
    # Save the unmodified `x` for a test that is conducted if `x` contains one
    # or more `NA` elements:
    x_orig <- x

    not_na <- which(!is.na(x))

    # Remove all `NA` values from the start and the end of `x` because `NA`s at
    # these particular locations cannot disprove that `x` is the kind of
    # sequence of interest. (They do mean that it cannot be proven, so the
    # function will return either `NA` or `FALSE`, depending on other factors.)
    x <- x[not_na[1L]:not_na[length(not_na)]]

    # The question `vignette("devtools")` states for missing values: are the
    # known values consistent with each other, given their index positions? For
    # linearity, that is whether every pair of neighboring known values implies
    # the same step per index. The gaps used to be bridged at a step of one
    # decimal unit instead, whatever the known values said, so
    # `c(1, NA, 5, 7)` -- linear at a step of 2 -- was `FALSE`.
    known <- which(!is.na(x))

    if (test_linear) {
      steps <- diff(x[known]) / diff(known)
      if (!all(dplyr::near(steps, steps[1L], tol = tolerance))) {
        return(FALSE)
      }
    }

    # The special tests, too, ask whether the known values already rule the
    # sequence out. They used to return `NA` early whenever fewer than three
    # values were known, and, for dispersion, whenever the central value was, so
    # `c(2, NA, 1)` might have been ascending and `c(1, 2, NA, 4, 5)` might have
    # been dispersed around `99`. A dispersed sequence has each pair of values
    # at mirrored positions centered on `from`, which a pair with both values
    # known can disprove; the central value is its own mirror image.
    # Monotonicity is disproven by the known values alone, gaps and all:
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
      if (!pass_test_special) {
        return(FALSE)
      }
    }

    return(NA)
  }

  # If desired, test `x` for linearity. The steps are compared with their signs,
  # so a zigzag like `c(1, 2, 1)` is not linear; their absolute values used to
  # be compared, which made it so:
  if (test_linear) {
    steps <- diff(x)
    if (!all(dplyr::near(steps, steps[1L], tol = tolerance))) {
      return(FALSE)
    }
  }

  # Interface for the special variant functions:
  if (!is.null(test_special)) {
    pass_test_special <- switch(
      test_special,
      "ascending" = is_seq_ascending_basic(x),
      "descending" = is_seq_descending_basic(x),
      "dispersed" = is_seq_dispersed_basic(x, from, tolerance)
    )
    if (!pass_test_special) {
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
#'   - `NA` if all elements are `NA`.
#'   - Otherwise, `NA` if some values substituted for the `NA`s would make `x`
#'   the kind of sequence in question, and `FALSE` if none would.
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

  # `is_seq_basic()` has made both of these numeric by the time it calls this.
  # (A string `from` used to be assigned to `x` here by mistake, so the call
  # failed with "non-numeric argument to binary operator".)
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
