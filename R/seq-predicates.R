
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
  x_seq <- x[x_seq] - x[x_seq + 1L]
  abs(x_seq[!is.na(x_seq)])
}


is_seq_linear_basic <- function(x) {
  if (length(x) < 3L) {
    return(TRUE)
  }
  # As the difference between each successive pair of values must be equal for
  # `x` to be a linear sequence, we can take the first pairwise difference and
  # test each other difference for equality with it. If any comparison turns out
  # unequal, `x` is not a linear sequence.
  diff_first <- x[2L] - x[1L]
  for (i in 3L:length(x)) {
    if (x[i] - x[i - 1L] != diff_first) {
      return(FALSE)
    }
  }
  TRUE
}

is_seq_ascending_basic <- function(x) {
  for (i in 1L:(length(x) - 1L)) {
    if (x[i + 1L] <= x[i]) {
      return(FALSE)
    }
  }
  TRUE
}


is_seq_descending_basic <- function(x) {
  for (i in 1L:(length(x) - 1L)) {
    if (x[i + 1L] >= x[i]) {
      return(FALSE)
    }
  }
  TRUE
}


# # Test any of the sequence functions interactively:
# x <- c(1, 2, NA, 4)
# tolerance <- .Machine$double.eps^0.5
# test_linear <- TRUE
# test_special <- NULL
# min_length <- NULL
# args_other <- NULL



# # Example input for dispersed sequences:
# x <- c(45, NA, 47, 48, 49, 50, 51, 52, 53, 54, NA)
# tolerance <- .Machine$double.eps^0.5
# test_linear <- TRUE
# test_special <- "dispersed"
# min_length <- 3L
# args_other <- list(from = 50)


# Non-exported workhorse API of all the sequence predicates:
is_seq_basic <- function(x, tolerance = .Machine$double.eps^0.5,
                         test_linear = TRUE, test_special = NULL,
                         min_length = NULL, args_other = NULL) {

  if (!is.null(test_special) && test_special == "dispersed") {
    # Without the `force()` call, the function may return `FALSE` early, even if
    # `from` was not supplied:
    force(args_other$from)

    # A dispersed sequence requires one central value, so the number of elements
    # in `x` must be odd:
    if (is_even(length(x))) {
      return(FALSE)
    }
  }

  if (!is_numeric_like(x)) {
    return(FALSE)
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

    # # Indices of `NA`s at the start and end of `x`:
    # n_na_start <- seq_len(not_na[1L] - 1L)
    # n_na_end   <- (not_na[length(not_na)] + 1L):n_x_orig

    n_na_start <- match(FALSE,     is.na(x_orig))  - 1L
    n_na_end   <- match(FALSE, rev(is.na(x_orig))) - 1L

    # Remove all `NA` values from the start and the end of `x` because `NA`s at
    # these particular locations cannot disprove that `x` is the kind of
    # sequence of interest. (They do mean that it cannot be proven, so the
    # function will return either `NA` or `FALSE`, depending on other factors.)
    x <- x[not_na[1L]:not_na[length(not_na)]]

    # Test separate from `x_has_na`: it checks whether there are `NA`s that are
    # not at the start or end of `x`.
    if (test_linear && !anyNA(x) && !is_seq_linear_basic(x)) {
      return(FALSE)
    }

    # If the removal of leading and / or trailing `NA` elements in `x` caused
    # the central value to shift, or if only one side from among left and right
    # had any `NA` values, the original `x` might not have been symmetrically
    # grouped around that value, and hence not a dispersed sequence. Otherwise,
    # the `NA`s leave it open and the result is unknown, i.e., `NA`.
    if (!is.null(test_special) && test_special == "dispersed") {
      x_central <- x_orig[index_central(x_orig)]
      if (!is.na(x_central) && x_central != args_other$from) {
        return(FALSE)
      }
      return(NA)
    }

    # Used within the for loop below to check whether the step size must be
    # negative:
    x_is_descending_basic <- is_seq_descending_basic(x[!is.na(x)])

    for (i in seq_along(x)) {
      if (is.na(x[i])) {
        index_lower <- 1L
        index_upper <- 1L
        while (is.na(x[i - index_lower])) {
          index_lower <- index_lower - 1L
        }
        while (is.na(x[i + index_upper])) {
          index_upper <- index_upper + 1L
        }
        seq_start <- x[i - index_lower]
        seq_end   <- x[i + index_upper]
        step <- step_size(c(seq_start, seq_end))

        # Descending sequences require a negative step size:
        if (x_is_descending_basic || seq_start > seq_end) {
          step <- -step
        }

        # Look here for `seq.default()` errors:
        seq_replacement <- seq(from = seq_start, to = seq_end, by = step)

        # Remove the first and the last element because these correspond to the
        # two next surrounding non-`NA` numbers rather than to the `NA`
        # subsequence, and therefore should not replace any `NA`s:
        seq_replacement <- seq_replacement[-1L]
        seq_replacement <- seq_replacement[-length(seq_replacement)]

        if (test_linear) {
          # In the first of these two cases, the replacement sequence is too
          # short to bridge the `NA` subsequence. In the second case, the
          # replacement sequence is longer than the subsequence of `NA`
          # elements, which invariably means that the numbers surrounding the
          # `NA`s are too far spaced out for there to be a linear sequence. In
          # either case...
          seq_replacement_has_wrong_length <-
            length(seq_replacement) == 0L ||
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
          x[i + ((index_lower:index_upper) - 1L)] <- seq_replacement
        )

      } # End of the `is.na(x[i])` condition
    }   # End of the for loop
  }     # End of the `x_has_na` condition

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
      "ascending"  = is_seq_ascending_basic(x),
      "descending" = is_seq_descending_basic(x),
      "dispersed"  = is_seq_dispersed_basic(x, args_other$from, tolerance)
      # TODO: MAKE THIS "DISPERSED" TEST ABLE TO ASSUME THAT `NA`S IN `x_orig`
      # ARE ACTUALLY DISPERSED VALUES! MAYBE USE `index_central()`'S INTERNAL
      # LOGIC AND BUILD UP ON IT.
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

is_seq_ascending <- function(x, test_linear = TRUE,
                             tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "ascending", min_length = 2L
  )
}



#' @rdname seq-predicates
#' @export

is_seq_descending <- function(x, test_linear = TRUE,
                              tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "descending", min_length = 2L
  )
}



#' @rdname seq-predicates
#' @export

is_seq_dispersed <- function(x, from, test_linear = TRUE,
                             tolerance = .Machine$double.eps^0.5) {
  is_seq_basic(
    x, tolerance, test_linear, test_special = "dispersed", min_length = 3L,
    args_other = list(from = from)
  )
}


# x <- 50 %>%
#   seq_disperse() %>%
#   as.numeric()
# x[2] <- NA
# x[length(x)] <- NA
# from <- 50
# tolerance <- .Machine$double.eps^0.5

# Helper, not exported:
is_seq_dispersed_basic <- function(x, from,
                                   tolerance = .Machine$double.eps^0.5) {

  if (is_even(length(x))) {
    return(FALSE)
  }

  if (!is.numeric(x)) {
    if (is_numeric_like(x)) {
      x <- as.numeric(x)
    } else {
      return(FALSE)
    }
  }

  if (!is.numeric(from)) {
    if (is_numeric_like(from)) {
      x <- as.numeric(from)
    } else {
      return(FALSE)
    }
  }

  index_central_x <- index_central(x)

  if (!dplyr::near(x[index_central_x], from, tolerance)) {
    return(FALSE)
  }

  dispersion_minus <- from - x[1L:(index_central_x - 1L)]
  dispersion_plus  <- from + x[(index_central_x + 1L):length(x)]

  from_reconstructed <- (dispersion_plus - rev(dispersion_minus)) / 2

  all(dplyr::near(from, from_reconstructed, tolerance))
}


# Helper, not exported:
fill_linear_sequence <- function(x) {
  # Find positions of non-NA values
  known_pos <- which(!is.na(x))

  # Need at least 2 known values to determine a linear sequence
  if (length(known_pos) < 2) {
    stop("At least 2 non-NA values are required")
  }

  # Calculate differences between consecutive known values
  diffs <- diff(x[known_pos]) / diff(known_pos)

  # Check if differences are constant (within floating point tolerance)
  if (!all(abs(diffs - diffs[1]) < .Machine$double.eps ^ 0.5)) {
    return(NULL)
  }

  # Calculate the common difference
  d <- diffs[1]

  # Calculate the first value based on the sequence
  first_known <- x[known_pos[1]]
  start_value <- first_known - (known_pos[1] - 1) * d

  # Generate the complete sequence
  seq(from = start_value, by = d, length.out = length(x))
}

