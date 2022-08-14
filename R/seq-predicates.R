

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
  x_seq <- seq_along(x)
  x_seq <- x[x_seq] - x[x_seq + 1]
  abs(x_seq[!is.na(x_seq)])
}



is_linear <- function(x, tolerance) {
  x_seq <- index_seq(x)
  x_seq <- dplyr::near(x_seq, min(x_seq), tol = tolerance)
  all(x_seq)
}



is_seq_ascending_basic <- function(x) {
  for (i in 1:(length(x) - 1)) {
    if (x[i + 1] <= x[i]) {
      return(FALSE)
    }
  }
  TRUE
}


is_seq_descending_basic <- function(x) {
  for (i in 1:(length(x) - 1)) {
    if (x[i + 1] >= x[i]) {
      return(FALSE)
    }
  }
  TRUE
}



# Non-exported workhorse API of all the sequence predicates:
is_seq_basic <- function(x, tolerance = .Machine$double.eps^0.5,
                         test_linear = TRUE, test_special = NULL,
                         min_length = NULL, args_other = NULL) {
  if (all(is.na(x))) {
    return(NA)
  }

  if (!is_numericish(x)) {
    return(FALSE)
  }

  if (!is.null(min_length) && length(x) < min_length) {
    return(FALSE)
  }

  if (length(x) == 1) {
    return(TRUE)
  }

  if (!is.null(test_special) && test_special == "dispersed") {
    # A dispersed sequence requires one central value, so the number of elements
    # in `x` must be odd:
    if (is_even(length(x))) {
      return(FALSE)
    }
    # Save the unmodified `x` for a test that is conducted if `x` contains one
    # or more `NA` elements:
    x_orig <- x
  }

  x_has_na <- any(is.na(x))

  if (x_has_na) {

    # These two `while`-loops remove all `NA` values from the start and the end
    # of `x` because `NA`s at these particular locations have no bearing on
    # whether or not `x` might represent a linear sequence:
    while (is.na(x[1])) {
      x <- x[-1]
    }
    while (is.na(x[length(x)])) {
      x <- x[-length(x)]
    }

    # If the removal of leading and / or trailing `NA` elements in `x` caused
    # the central value to shift, or if only one side from among left and right
    # had any `NA` values, the original `x` was not symmetrically grouped around
    # that value, and hence not a dispersed sequence:
    if (!is.null(test_special) && test_special == "dispersed") {
      diff_central_index <-
        !dplyr::near(args_other$from, x[index_central(x)], tolerance)
      one_sided_na <- (!is.na(x_orig[1])) || (!is.na(x_orig[length(x_orig)]))
      if (is_even(length(x)) || diff_central_index || one_sided_na) {
        return(FALSE)
      }
    }

    for (i in seq_along(x)) {
      if (is.na(x[i])) {
        index_lower <- 1
        index_upper <- 1
        while (is.na(x[i - index_lower])) {
          index_lower <- index_lower - 1
        }
        while (is.na(x[i + index_upper])) {
          index_upper <- index_upper + 1
        }

        seq_start <- x[i - index_lower]
        seq_end   <- x[i + index_upper]

        step <- step_size(c(seq_start, seq_end))

        seq_replacement <- seq(from = seq_start, to = seq_end, by = step)

        # Remove the first and the last element because these correspond to the
        # two next surrounding non-`NA` numbers rather than to the `NA`
        # subsequence, and therefore should not replace any `NA`s:
        seq_replacement <- seq_replacement[-1]
        seq_replacement <- seq_replacement[-length(seq_replacement)]

        if (test_linear) {
          # In the first of these two cases, the replacement sequence is too
          # short to bridge the `NA` subsequence. In the second case, the
          # replacement sequence is longer than the subsequence of `NA`
          # elements, which invariably means that the numbers surrounding the
          # `NA`s are too far spaced out for there to be a linear sequence. In
          # either case...
          seq_replacement_has_wrong_length <- length(seq_replacement) == 0 ||
            length(seq_replacement) > length(index_lower:index_upper)

          # ...an error is thrown:
          if (seq_replacement_has_wrong_length) {
            return(FALSE)
          }
        }

        # Substitute the replacement sequence for `NA` elements. Warnings are
        # suppressed because the lengths will only differ in an unproblematic
        # case -- `x` is non-linear and `test_linear` is `FALSE`, i.e., the user
        # only wants one of the special tests but not the test for linearity:
        suppressWarnings(
          x[i + ((index_lower:index_upper) - 1)] <- seq_replacement
        )

      } # End of the `is.na(x[i])` condition
    }   # End of the for loop

  }     # End of the `x_has_na` condition

  # If desired, test `x` -- as passed to the function or as partly reconstructed
  # in the for loop above -- for linearity:
  if (test_linear) {
    x_seq <- index_seq(x)
    x_seq <- dplyr::near(x_seq, min(x_seq), tol = tolerance)
    pass_test <- all(x_seq)
    if (!pass_test) {
      return(FALSE)
    }
  } else {
    pass_test <- TRUE
  }

  # Interface for the special variant functions:
  if (!is.null(test_special)) {
    pass_test_special <- switch (
      test_special,
      "ascending"  = is_seq_ascending_basic(x),
      "descending" = is_seq_descending_basic(x),
      "dispersed"  = is_seq_dispersed_basic(x, args_other$from, tolerance)
    )
    pass_test <- pass_test && pass_test_special
  }

  if (pass_test) {

    if (x_has_na) {
      return(NA)
    } else {
      return(TRUE)
    }

  } else {
    return(FALSE)
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
#' `NA` elements of `x` are handled in a nuanced way. See *Value* section here
#' and the examples in `vignette("infrastructure")`, section *NA handling*.

#' @param x Numeric or coercible to numeric. Vector to be tested.
#' @param from Numeric or coercible to numeric. Only in `is_seq_dispersed()`. It
#'   will test whether `from` is at the center of `x`, and if every pair of
#'   other values is equidistant to it.
#' @param test_linear Boolean. Should `x` be tested for linearity, as in
#'   `is_seq_linear()`? Default is `TRUE`.
#' @param tolerance Numeric. Tolerance of comparison between numbers such as the
#'   difference between individual `x` values and the minimal difference in
#'   linearity testing. Default is circa 0.000000015 (1.490116e-08), as in
#'   `dplyr::near()`.

#' @return Boolean. If `x` contains at least one `NA` value, the functions
#'   return `NA` or `FALSE`, depending on the context:
#'   - If all elements of `x` are `NA`, the functions return `NA`.
#'   - If some but not all elements are `NA`, they check if `x` *might* be a
#'   linear sequence; i.e., if it is linear after the `NA`s were replaced by
#'   appropriate values. If so, they return `NA`; otherwise, they return
#'   `FALSE`.

#' @seealso `validate::is_linear_sequence()`, which is much like
#'   `is_seq_linear()` but more permissive with `NA` values. It comes with some
#'   other features, such as support for date-times.

#' @export

#' @examples
#' # These are linear sequences...
#' is_seq_linear(x = 3:7)
#' is_seq_linear(x = c(3:7, 8))
#'
#' # ...but this isn't:
#' is_seq_linear(x = c(3:7, 9))
#'
#' # All other `is_seq_*()` functions
#' # test for linearity by default
#' # besides conducting their
#' # individual tests:
#' is_seq_ascending(x = c(1, 2, 5))
#' is_seq_ascending(x = c(1, 2, 5), test_linear = FALSE)
#'
#' is_seq_descending(x = c(9, 7, 2))
#' is_seq_descending(x = c(9, 7, 2), test_linear = FALSE)
#'
#' is_seq_dispersed(x = c(2, 3 ,5, 7, 8), from = 5)
#' is_seq_dispersed(x = c(2, 3 ,5, 7, 8), from = 5, test_linear = FALSE)
#'
#' # These fail the respective
#' # individual test even
#' # without linearity testing:
#' is_seq_ascending(x = c(1, 7, 4), test_linear = FALSE)
#' is_seq_descending(x = c(9, 15, 3), test_linear = FALSE)
#' is_seq_dispersed(1:10, from = 5, test_linear = FALSE)




is_seq_linear <- function(x, tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(x, tolerance)
}



#' @export
#' @rdname is_seq_linear

is_seq_ascending <- function(x, test_linear = TRUE,
                             tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "ascending", min_length = 2
  )
}



#' @export
#' @rdname is_seq_linear

is_seq_descending <- function(x, test_linear = TRUE,
                              tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "descending", min_length = 2
  )
}



#' @export
#' @rdname is_seq_linear

is_seq_dispersed <- function(x, from, test_linear = TRUE,
                             tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "dispersed", min_length = 3,
    args_other = list(from = from)
  )
}




# Helper, not exported:
is_seq_dispersed_basic <- function(x, from,
                                   tolerance = .Machine$double.eps^0.5) {

  # Without `force(from)`, the function may return `FALSE` early, even if `from`
  # was not supplied:
  force(from)

  if (is_even(length(x))) {
    return(FALSE)
  }

  if (!is.numeric(x)) {
    if (is_numericish(x)) {
      x <- as.numeric(x)
    } else {
      return(FALSE)
    }
  }

  if (!is.numeric(from)) {
    if (is_numericish(from)) {
      x <- as.numeric(from)
    } else {
      return(FALSE)
    }
  }

  index_central_x <- index_central(x)

  if (!dplyr::near(x[index_central_x], from, tolerance)) {
    return(FALSE)
  }

  dispersion_minus <- from - x[1:(index_central_x - 1)]
  dispersion_plus  <- from + x[(index_central_x + 1):length(x)]

  from_reconstructed <- (dispersion_plus - rev(dispersion_minus)) / 2

  all(dplyr::near(from, from_reconstructed, tolerance))
}



