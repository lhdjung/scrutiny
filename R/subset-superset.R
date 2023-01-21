

#' Test for subsets, supersets, and equal sets
#'
#' @description Predicate functions that take a vector and test whether it has
#'   some particular relation to another vector. That second vector is entered
#'   in either of three ways --
#'
#'   \strong{Enter it directly (basic functions):}
#'
#'   `is_subset_of()` tests if a vector is a subset of another vector; i.e., if
#'   all its elements are contained in the second one. `is_superset_of()` does
#'   the reverse: It tests if the first vector contains all elements of the
#'   second one. `is_equal_set()` tests if both vectors have exactly the same
#'   values.
#'
#'   \strong{Enter its values:}
#'
#'   `is_subset_of_vals()`, `is_superset_of_vals()`, and `is_equal_set_vals()`
#'   are variants that each take a single vector plus any number of other
#'   arguments. These are treated like elements of the second vector in the
#'   basic functions above.
#'
#'   \strong{Enter multiple vectors that jointly contain its values:}
#'
#'   Finally, `is_subset_of_vecs()`, `is_superset_of_vecs()`, and
#'   `is_equal_set_vecs()` take one vector plus any number of other vectors and
#'   treat their elements (!) like elements of a second vector in the basic
#'   functions above.
#'
#'   Each `is_subset*()` function has an `is_proper_subset*()` variant. These
#'   variants also test whether the sets are unequal, so that `x` is a subset of
#'   `y` but `y` is not a subset of `x`. The same applies to `is_superset*()`
#'   functions and their `is_proper_superset*()` variants.
#'
#' @details The `*_vals()` variants are meant for flexible, interactive
#'   subset/superset testing. That is, in order to test whether certain values
#'   collectively fulfill the role of the second vector, you can just add them
#'   to the function call.
#'
#'   The `*_vecs()` variants likewise offer flexibility, but also bridge the gap
#'   between vectors and values contained in them.
#'
#'   All functions simply check if values are present, regardless of how often a
#'   value occurs. In other words, they look for types but don't count tokens.
#'
#' @param x A vector.
#' @param y A vector. Only in the basic functions, not those with `*_vals()` or
#'   `*_vecs()`.
#' @param ... In the `*_vals()` functions, any number of values `x` might
#'   contain; in the `*_vecs()` functions, any number of vectors the elements of
#'   which `x` might contain.
#'
#' @return A single Boolean value. `TRUE` if the respective test was passed,
#'   `FALSE` otherwise.
#'
#' @include utils.R
#'
#' @export
#'
#' @name subset-superset
#'
#' @examples
#' # Define example vectors:
#' ab <- c("a", "b")
#' abc <- c("a", "b", "c")
#' abcde <- c("a", "b", "c", "d", "e")
#'
#' # `is_subset_of()` tests if a vector is
#' # completely covered by another one:
#' abc %>% is_subset_of(ab)
#' abc %>% is_subset_of(abc)
#' abc %>% is_subset_of(abcde)
#'
#' # To the contrary, `is_superset_of()` tests if the
#' # first vector completely covers the second one:
#' abc %>% is_superset_of(ab)
#' abc %>% is_superset_of(abc)
#' abc %>% is_superset_of(abcde)
#'
#' # `is_equal_set()` tests both of the above --
#' # i.e., if both vectors have exactly the
#' # same values:
#' abc %>% is_equal_set(ab)
#' abc %>% is_equal_set(abc)
#' abc %>% is_equal_set(abcde)
#'
#' # Each of the three functions has a `*_vals()` variant
#' # that doesn't take a second vector like the first
#' # one, but any number of other arguments. These are
#' # jointly treated like the elements of the second
#' # vector in the basic functions:
#' abc %>% is_subset_of_vals("a", "b")
#' abc %>% is_subset_of_vals("a", "b", "c")
#' abc %>% is_subset_of_vals("a", "b", "c", "d", "e")
#'
#' # (... and likewise for supersets and equal sets.)



# Basic two-vector functions ----------------------------------------------

#' @rdname subset-superset
#' @export

is_subset_of <- function(x, y) {
  x_rest <- x[!x %in% y]
  length(x_rest) == 0L
}


#' @rdname subset-superset
#' @export

is_superset_of <- function(x, y) {
  y_rest <- y[!y %in% x]
  length(y_rest) == 0L
}


#' @rdname subset-superset
#' @export

is_equal_set <- function(x, y) {
  is_subset_of(x, y) && is_superset_of(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_subset_of <- function(x, y) {
  is_subset_of(x, y) && !is_superset_of(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_superset_of <- function(x, y) {
  is_superset_of(x, y) && !is_subset_of(x, y)
}



# With multiple arguments treated as values -------------------------------

#' @rdname subset-superset
#' @export

is_subset_of_vals <- function(x, ...) {
  y <- rlang::enexprs(...)
  is_subset_of(x, y)
}



#' @rdname subset-superset
#' @export

is_superset_of_vals <- function(x, ...) {
  y <- rlang::enexprs(...)
  is_superset_of(x, y)
}



#' @rdname subset-superset
#' @export

is_equal_set_vals <- function(x, ...) {
  y <- rlang::enexprs(...)
  is_equal_set(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_subset_of_vals <- function(x, ...) {
  y <- rlang::enexprs(...)
  is_proper_subset_of(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_superset_of_vals <- function(x, ...) {
  y <- rlang::enexprs(...)
  is_proper_superset_of(x, y)
}




# With multiple arguments that might contain values -----------------------

# Note: The helper function `straighten_out()` can be found in the utils.R file.

#' @rdname subset-superset
#' @export

is_subset_of_vecs <- function(x, ...) {
  y <- straighten_out(...)
  is_subset_of(x, y)
}



#' @rdname subset-superset
#' @export

is_superset_of_vecs <- function(x, ...) {
  y <- straighten_out(...)
  is_superset_of(x, y)
}



#' @rdname subset-superset
#' @export

is_equal_set_vecs <- function(x, ...) {
  y <- straighten_out(...)
  is_equal_set(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_subset_of_vecs <- function(x, ...) {
  y <- straighten_out(...)
  is_proper_subset_of(x, y)
}


#' @rdname subset-superset
#' @export

is_proper_superset_of_vecs <- function(x, ...) {
  y <- straighten_out(...)
  is_proper_superset_of(x, y)
}


