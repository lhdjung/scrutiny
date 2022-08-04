

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



might_be_linear <- function(x, tolerance = .Machine$double.eps^0.5) {
  x_seq <- index_seq(x)
  x_seq <- dplyr::near(x_seq, min(x_seq), tol = tolerance)
  all(x_seq)
}



#' Is a vector a linear sequence?
#'
#' @description `is_seq_linear()` checks if a vector `x` has these properties:
#'   - It is numeric or coercible to numeric.
#'   - Each successive element differs from the previous one by some constant
#'   amount.
#'
#' The variants `is_seq_linear_ascending()` and `is_seq_linear_descending()` are
#' more strict: They also check if the step size is positive or negative,
#' respectively.
#'
#' @param x Numeric or coercible to numeric. Vector to be tested.
#' @param tolerance Numeric. Tolerance of comparison between the distances
#'   between individual `x` values and the minimal distance. Default is circa
#'   0.000000015 (1.490116e-08), as in `dplyr::near()`.

#' @return Boolean. `NA` elements of `x` are handled in a nuanced way:
#'   - If all elements of `x` are `NA`, the functions return `NA.`
#'   - If some but not all elements are `NA`, they check if `x` *might* be a
#'   linear sequence; i.e., if it is linear after the `NA`s are replaced by
#'   appropriate values. If so, they return `NA`; otherwise, they return
#'   `FALSE`.

#' @export
#'
#' @examples


is_seq_linear_internal <- function(x, tolerance = .Machine$double.eps^0.5,
                                   test_special = NULL) {
  if (all(is.na(x))) {
    return(NA)
  }

  if (!is_numericish(x)) {
    return(FALSE)
  }

  if (length(x) == 1) {
    return(TRUE)
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
        seq_replacement <- seq_replacement[-1]
        seq_replacement <- seq_replacement[-length(seq_replacement)]

        # In this case, the replacement sequence is longer than the subsequence
        # of `NA` elements, which always means that the numbers surrounding the
        # `NA`s are too far spaced out for there to be a linear sequence:
        if (length(seq_replacement) > length(index_lower:index_upper)) {
          return(FALSE)
        }

        x[i + ((index_lower:index_upper) - 1)] <- seq_replacement
      }
    }

    # if (might_be_linear(x, tolerance)) {
    #   return(NA)
    # }
    #
    # return(FALSE)
  }

  x_passes_test <- might_be_linear(x, tolerance)

  # Interface for the special variant functions:
  if (!is.null(test_special)) {
    x_passes_test_special <- switch (
      test_special,
      "ascending"  = x[2] - x[1] > 0,
      "descending" = x[2] - x[1] < 0
    )
    x_passes_test <- x_passes_test && x_passes_test_special
  }

  if (x_passes_test) {
    if (x_has_na) {
      return(NA)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }

}


# if (test_special == "ascending") {
#   x_is_ascending <- x[2] - x[1] > 0
#   return(x_is_ascending)
# }
# if (test_special == "descending") {
#   x_is_descending <- x[2] - x[1] < 0
#   return(x_is_descending)
# }



is_seq_linear <- function(x, tolerance = .Machine$double.eps^0.5) {
  is_seq_linear_internal(x, tolerance, test_special = NULL)
}

is_seq_linear_ascending <- function(x, tolerance = .Machine$double.eps^0.5) {
  is_seq_linear_internal(x, tolerance, test_special = "ascending")
}

is_seq_linear_descending <- function(x, tolerance = .Machine$double.eps^0.5) {
  is_seq_linear_internal(x, tolerance, test_special = "descending")
}




# # Non-exported helper for the `is_seq_linear_ascending()` and
# # `is_seq_linear_descending()` variants further below:
# check_seq_linear_special <- function(x, tolerance, description) {
#   if (all(is.na(x))) {
#     return(NA)
#   }
#
#   # This says `isFALSE()` rather than simply `!` because its argument might be
#   # `NA`, and the present condition is not meant to handle such cases:
#   if (isFALSE(is_seq_linear(x, tolerance))) {
#     return(FALSE)
#   }
#
#   if (!is.numeric(x)) {
#     x <- as.numeric(x)
#   }
#
#   if (length(x) == 1) {
#     cli::cli_warn(c(
#       "{description} order couldn't be determined.",
#       "x" = "`x` (`{x}`) has length 1.",
#       ">" = "Returning `NA`."
#     ))
#     return(NA)
#   }
#
#   return(x)
# }



#' @export
#' @rdname is_seq_linear

# is_seq_linear_ascending <- function(x, tolerance = .Machine$double.eps^0.5) {
#   x <- check_seq_linear_special(x, tolerance, "Ascending")
#   if (!is.numeric(x)) {
#     return(x)
#   }
#   x[2] - x[1] > 0
# }



#' @export
#' @rdname is_seq_linear

# is_seq_linear_descending <- function(x, tolerance = .Machine$double.eps^0.5) {
#   x <- check_seq_linear_special(x, tolerance, "Descending")
#   if (!is.numeric(x)) {
#     return(x)
#   }
#   x[2] - x[1] < 0
# }

