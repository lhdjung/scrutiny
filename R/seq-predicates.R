# Helpers for `function_map_seq()` as well as its assorted `reverse_*()` and
# `summarize_*()` functions:

# `index_seq()` takes a vector that's (1) numeric or string coercible to
# numeric, and (2) that is either a continuous sequence of numbers (step size:
# 1) or that would be such a sequence if not for exactly one single missing case
# -- not in the sense of `NA`, but a sequence of two numbers where the second is
# the first plus 2, so that one would expect an intermediate number in the
# middle. This number can be identified as the "index case" in an original
# sequence that dropped it at some point.

# The function returns a sequence of `1` values for a continuous sequence, and
# such a sequence with a single `2` value strewn in for a sequence with a single
# missing case. The `2`, if present, has the same index as the last value before
# the index case in `x`.
index_seq <- function(x) {
  if (!is.numeric(x)) {
    x <- as.numeric(x)
  }
  steps <- abs(diff(x))
  steps[!is.na(steps)]
}


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

  x_has_na <- anyNA(x)

  if (x_has_na) {
    # Save the unmodified `x` for a test that is conducted if `x` contains one
    # or more `NA` elements:
    x_orig <- x
    n_x_orig <- length(x)

    not_na <- which(!is.na(x))

    # Need at least three known values:
    if (length(not_na) < 3L) {
      return(NA)
    }

    n_na_start <- match(FALSE, is.na(x_orig)) - 1L
    n_na_end <- match(FALSE, rev(is.na(x_orig))) - 1L

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

    # If the removal of leading and / or trailing `NA` elements in `x` caused
    # the central value to shift, or if only one side from among left and right
    # had any `NA` values, the original `x` might not have been symmetrically
    # grouped around that value, and hence not a dispersed sequence. Otherwise,
    # the `NA`s leave it open and the result is unknown, i.e., `NA`.
    if (!is.null(test_special) && test_special == "dispersed") {
      x_central <- x_orig[index_central(x_orig)]
      if (!is.na(x_central) && x_central != from) {
        return(FALSE)
      }
      return(NA)
    }

    # Fill the gaps by linear interpolation between their known neighbors, so
    # that the special tests below see the values that a linear sequence would
    # have there. Without `test_linear`, this is the same answer the known
    # values give on their own -- an interpolated value never breaks a monotone
    # run between two known ones:
    x <- stats::approx(known, x[known], xout = seq_along(x))$y
  } # End of the `x_has_na` condition

  # If desired, test `x` -- as passed to the function or as partly reconstructed
  # in the for loop above -- for linearity:
  if (test_linear) {
    x_seq <- index_seq(x)
    pass_test_linear <- all(dplyr::near(x_seq, min(x_seq), tol = tolerance))
    if (!pass_test_linear) {
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

  if (x_has_na) {
    NA
  } else {
    TRUE
  }
}


#' Is a vector a certain kind of sequence?
#'
#' @description Predicate functions that test whether `x` is a numeric vector
#'   (or coercible to numeric) with some special properties:

#'   - `is_seq_linear()` tests whether every two consecutive elements of `x`
#'   differ by some constant amount.

#'   - `is_seq_ascending()` and `is_seq_descending()` test whether the
#'   difference between every two consecutive values is positive or negative,
#'   respectively. `is_seq_dispersed()` tests whether `x` values are grouped
#'   around a specific central value, `from`, with the same distance to both
#'   sides per value pair. By default (`test_linear = TRUE`), these functions
#'   also test for linearity, like `is_seq_linear()`.
#'
#' `NA` elements of `x` are handled in a nuanced way. See *Value* section below
#' and the examples in `vignette("devtools")`, section *NA handling*.

#' @param x Numeric or coercible to numeric, as determined by
#'   `is_numeric_like()`. Vector to be tested.
#' @param from Numeric or coercible to numeric. Only in `is_seq_dispersed()`. It
#'   will test whether `from` is at the center of `x`, and if every pair of
#'   other values is equidistant to it.
#' @param test_linear Logical. In functions other than `is_seq_linear()`, should
#'   `x` also be tested for linearity? Default is `TRUE`.
#' @param tolerance Numeric. Tolerance of comparison between numbers when
#'   testing. Default is circa 0.000000015 (1.490116e-08), as in
#'   `dplyr::near()`.

#' @return A single logical value. If `x` contains at least one `NA` element,
#'   the functions return either `NA` or `FALSE`:
#'   - If all elements of `x` are `NA`, the functions return `NA`.
#'   - If some but not all elements are `NA`, they check if `x` *might* be a
#'   sequence of the kind in question: Is it a linear (and / or ascending, etc.)
#'   sequence after the `NA`s were replaced by appropriate values? If so, they
#'   return `NA`; otherwise, they return `FALSE`.

#' @seealso `validate::is_linear_sequence()`, which is much like
#'   `is_seq_linear()` but more permissive with `NA` values. It comes with some
#'   additional features, such as support for date-times.

#' @export
#'
#' @name seq-predicates

#' @examples
#' # These are linear sequences...
#' is_seq_linear(x = 3:7)
#' is_seq_linear(x = c(3:7, 8))
#'
#' # ...but these aren't:
#' is_seq_linear(x = c(3:7, 9))
#' is_seq_linear(x = c(10, 3:7))
#'
#' # All other `is_seq_*()` functions
#' # also test for linearity by default:
#' is_seq_ascending(x = c(2, 7, 9))
#' is_seq_ascending(x = c(2, 7, 9), test_linear = FALSE)
#'
#' is_seq_descending(x = c(9, 7, 2))
#' is_seq_descending(x = c(9, 7, 2), test_linear = FALSE)
#'
#' is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5)
#' is_seq_dispersed(x = c(2, 3, 5, 7, 8), from = 5, test_linear = FALSE)
#'
#' # These fail their respective
#' # individual test even
#' # without linearity testing:
#' is_seq_ascending(x = c(1, 7, 4), test_linear = FALSE)
#' is_seq_descending(x = c(9, 15, 3), test_linear = FALSE)
#' is_seq_dispersed(1:10, from = 5, test_linear = FALSE)

#' @rdname seq-predicates
#' @export

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
