
#' Combine vector elements to given length
#'
#' @description *Note: This function is not yet part of an established workflow,
#'   but will be key to implementing GRIMMER (granularity-related inconsistency
#'   of means mapped to error repeats), i.e., GRIM-testing applied to standard
#'   deviations instead of means. At that point, it will essentially be a helper
#'   function, so it is not exported now, and unlikely to be exported in the
#'   future.*
#'
#'   `combine_elements()` combines a vector's elements with each other. The
#'   length of the combinations is explicitly specified.
#'
#'   Combinations are represented by rows in the resulting data frame. Each row
#'   is a unique combination (unique in terms of its values themselves, not just
#'   the sequence of its values).
#'
#' @details This function is part of scrutiny's GRIMMER implementation. It
#'   closely mimics the behavior of the Python function
#'   \href{https://docs.python.org/3/library/itertools.html#itertools.combinations_with_replacement}{`itertools.combinations_with_replacement()`}.
#'    The most important difference is that `combine_elements()` organizes
#'   results in a data frame, with rows corresponding to Python's tuples. The
#'   row order might differ from the order of tuples in the original.
#'
#' @param x A vector with elements to combine.
#' @param n Integer of length 1. Number of elements in each combination, and
#'   thus, number of columns in the resulting data frame.
#' @param prefix String. Start of every column in the output tibble, before the
#'   column number. Default is `"V"`.
#'
#' @return A tibble (data frame).
#'
#' @seealso `expand.grid()`, which the function wraps, the
#'   \href{https://purrr.tidyverse.org/reference/cross.html}{`cross` family of
#'   functions} from purrr and, for functional programming with combinations,
#'   the \href{https://crossmap.rossellhayes.com/index.html}{crossmap} package.


# @examples
# # Define example vector:
# abc <- c("a", "b", "c")
#
# # Get combinations of vector `x` with length `n`,
# # with one combination per row:
# combine_elements(x = abc, n = 2)
#
# # `n` is the length of each combination, and
# # therefore the number of columns. Indirectly,
# # it determines the number of combination-rows:
# combine_elements(x = abc, n = 3)



# Note: The internal helper functions `remove_equivalent_rows()` and
# `reverse_column_order()` can be found in the utils.R file.


combine_elements <- function(x, n, prefix = "V") {

  # As `n` determines the length of combinations through its (single) value
  # rather than through its own length, the length needs to be 1:
  if (length(n) != 1) {
    cli::cli_abort(c(
      "`n` has length {length(n)}",
      "x" = "It needs to have length 1."
    ))
  }

  # Generate combinations, removing rows with the same value sets (regardless of
  # their order) and reversing the order of columns to accord with the behavior
  # of the Python function `itertools.combinations_with_replacement()`:
  out <- rep(list(x), n) %>%
    expand.grid(stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    remove_equivalent_rows() %>%
    reverse_column_order()

  # (The last two functions called above are from the utils.R file.)

  # Following the above column order reversal, replace the column names so that
  # column name suffix numbers run from 1 to `n`, not the reverse:
  colnames(out) <- paste0(prefix, 1:n)

  # Return the output data frame:
  out
}


# # Alternative code for renaming the columns:
# colnames_orig <- colnames(out)
# out <- reverse_column_order(out)
# colnames(out) <- colnames_orig
