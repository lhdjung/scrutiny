# Helpers for the sequence predicates below:

is_seq_ascending_basic <- function(x) {
  all(diff(x) > 0)
}


is_seq_descending_basic <- function(x) {
  all(diff(x) < 0)
}


# Like `dplyr::near()`, but a `tolerance` of 0 means exact, and no tolerance is
# finer than the rounding error of arithmetic on numbers as large as `values`:
# steps between values of about 1e8 are off by 1e-8 already, which the default
# would reject. A step or center is off by at most two units in the last place
# of the largest value; a step of one unit in the 15th significant digit is at
# least 4.5 of these, so any wider floor merges such steps. `NA` for an
# infinity, as `near(Inf, Inf)` is:
near_seq <- function(x, y, tolerance, values, ulps = 2) {
  magnitude <- max(abs(values[is.finite(values)]), 0)
  ulp <- if (magnitude > 0) 2^(floor(log2(magnitude)) - 52) else 0
  abs(x - y) <= max(tolerance, ulps * ulp)
}


# A string as a number, as `is_numeric_like()` reads it: `as.numeric()` only
# trims ASCII whitespace, so a number next to a non-breaking space would be
# accepted as one there and then be missing here.
as_numeric_seq <- function(x) {
  if (is.character(x)) {
    x <- trimws(x, whitespace = "[\\h\\v]")
  }
  suppressWarnings(as.numeric(x))
}


# The sequence predicates take numbers, or strings or factors that may show
# them, such as `"1.10"` with its trailing zero. Anything else is a mistake in
# the call, not a sequence that fails the test: a data frame or a list would
# be judged by its columns or elements, and `TRUE` would be 1. A logical vector
# of `NA`s only is how R writes values that are missing altogether:
check_seq_type <- function(x, name) {
  allowed <- is.null(dim(x)) &&
    (is.numeric(x) ||
      is.character(x) ||
      is.factor(x) ||
      (is.logical(x) && all(is.na(x))))
  if (!allowed) {
    abort_in_export(
      "`{name}` must be a numeric, character, or factor vector.",
      "x" = "It is {.obj_type_friendly {x}}."
    )
  }
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
  check_seq_type(x, "x")
  # A missing value here would fail in `if ()` or make every test `FALSE`:
  check_flag(test_linear)
  if (
    !is.numeric(tolerance) ||
      length(tolerance) != 1L ||
      !isTRUE(tolerance >= 0)
  ) {
    abort_in_export(
      "`tolerance` must be a single non-negative number.",
      "x" = "It is {wrap_in_quotes_or_backticks(tolerance)}."
    )
  }

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
    from <- as_numeric_seq(from)

    # No values are centered on an infinity, so none could make `x` dispersed:
    if (is.infinite(from)) {
      return(FALSE)
    }
  }

  # Too short for the sequence in question, whatever the values:
  if (!is.null(min_length) && length(x) < min_length) {
    return(FALSE)
  }

  # A vector of all `NA`s leaves the question open, whatever its type -- unless
  # no values could close it: any one or two numbers are linear. A missing
  # string stands for a missing number, as the string `"NA"` does below.
  # `is_numeric_like()` is `FALSE` for a logical vector, which includes `c(NA,
  # NA, NA)`, and `NA` for a character one, which the `if` below can't take:
  if (length(x) > 0L && all(is.na(x))) {
    linear_anyway <- is.null(test_special) && length(x) <= 2L
    return(if (linear_anyway) TRUE else NA)
  }

  # The only logical vector left is an empty one, which is `numeric()`:
  if (is.logical(x)) {
    x <- numeric()
  }

  if (isFALSE(is_numeric_like(x))) {
    return(FALSE)
  }

  # Everything below is arithmetic on `x`. A factor is read by its labels, not
  # its integer codes. Integers become doubles, too: a difference of integers
  # overflows to `NA` beyond about 2.1e9.
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- as_numeric_seq(x)

  if (length(x) == 1L) {
    return(TRUE)
  }

  if (anyNA(x)) {
    # Whatever the missing value, two values are linear:
    if (is.null(test_special) && length(x) == 2L) {
      return(TRUE)
    }

    x_orig <- x

    not_na <- which(!is.na(x))

    # Strings such as `"NA"` and `"NaN"` only become missing here, so they can
    # leave no known value even past the all-`NA` check above:
    if (length(not_na) == 0L) {
      return(NA)
    }

    # Leading and trailing `NA`s cannot disprove that `x` is the kind of
    # sequence in question, only keep it from being proven:
    x <- x[not_na[1L]:not_na[length(not_na)]]

    # As in `vignette("devtools")`: are the known values consistent with each
    # other, given their positions? For linearity, every pair of neighboring
    # known values must imply the same step per index, so `c(1, NA, 5, 7)` can
    # be linear:
    known <- which(!is.na(x))

    # An infinity among more than two values rules linearity out, as below,
    # even if it is the only known value:
    if (test_linear) {
      steps <- diff(x[known]) / diff(known)
      if (
        (length(x_orig) > 2L && any(is.infinite(x[known]))) ||
          !isTRUE(all(near_seq(steps, steps[1L], tolerance, x)))
      ) {
        return(FALSE)
      }
    }

    # The special tests, too, ask whether the known values already rule the
    # sequence out. Monotonicity is disproven by the known values alone, and by
    # a missing value beyond an infinity, which no number could fill: the ends
    # are padded with the infinity that a leading or trailing `NA` would have
    # to pass. A dispersed sequence has each mirrored pair of values centered
    # on `from`, which a fully known pair can disprove; the central value is
    # its own mirror image:
    if (!is.null(test_special)) {
      na_first <- is.na(x_orig[1L])
      na_last <- is.na(x_orig[length(x_orig)])
      pass_test_special <- switch(
        test_special,
        "ascending" = is_seq_ascending_basic(
          c(if (na_first) -Inf, x[known], if (na_last) Inf)
        ),
        "descending" = is_seq_descending_basic(
          c(if (na_first) Inf, x[known], if (na_last) -Inf)
        ),
        "dispersed" = {
          # No number is centered with an infinity on a finite `from`, so a
          # known infinity rules it out, as does an infinite `from`:
          centers <- (x_orig + rev(x_orig)) / 2
          pass_pairs <- is.finite(from) &&
            !any(is.infinite(x_orig)) &&
            all(near_seq(centers[!is.na(centers)], from, tolerance, c(x, from)))
          # Two known values of a linear sequence fix its step, and so its
          # central value, even if no mirrored pair is known in full:
          known_orig <- which(!is.na(x_orig))
          if (test_linear && length(known_orig) >= 2L) {
            step <- steps[1L]
            steps_to_center <- (length(x_orig) + 1L) / 2L - known_orig[1L]
            center <- x_orig[known_orig[1L]] + step * steps_to_center
            # The rounding error of the step is multiplied along the way, so
            # the floor of the tolerance is wider by as much. Otherwise, a
            # `tolerance` of 0 would rule out a dispersed sequence:
            pass_pairs &&
              near_seq(
                center,
                from,
                tolerance,
                c(x, from),
                ulps = 2 * (abs(steps_to_center) + 1)
              )
          } else {
            pass_pairs
          }
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
  # Any two values are linear, even with an infinity among them. Among more,
  # an infinity makes a step `Inf - Inf`, which is `NaN` and equal to no other
  # step, hence `isTRUE()`, here and in the other tests:
  if (test_linear && length(x) > 2L) {
    steps <- diff(x)
    if (!isTRUE(all(near_seq(steps, steps[1L], tolerance, x)))) {
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
#' @param x Numeric, character, or factor vector. Strings and factor labels
#'   count as numbers as determined by [`is_numeric_like()`]; any that are not
#'   make the result `FALSE`. A logical vector is accepted only if all its
#'   values are `NA`. Any other type, such as a list or a data frame, is an
#'   error.
#' @param from Numeric, character, or factor, like `x`. Only in
#'   `is_seq_dispersed()`. The value `x` should be centered on.
#' @param test_linear Logical. Should linearity be tested as well? Default is
#'   `TRUE`. Not in `is_seq_linear()`.
#' @param tolerance Numeric. Tolerance for comparing values. Default is about
#'   `1.5e-8`, as in [`dplyr::near()`]. Any tolerance is widened to the
#'   rounding error of arithmetic on the values, at most about `4e-16` times
#'   the largest, so `0` allows that error only, and large values such as
#'   `123456789.01` are compared as precisely as doubles can.
#'
#' @return Logical (length 1). If `x` contains `NA`:
#'   - `NA` if some values substituted for the `NA`s would make `x` the kind of
#'   sequence in question and others would not; `FALSE` if none would, e.g.,
#'   because `x` is too short for it (two values for `is_seq_ascending()` and
#'   `is_seq_descending()`, three for `is_seq_dispersed()`); and `TRUE` if all
#'   would, as any one or two values are linear.
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
  check_seq_type(x, "x")
  check_length(from, 1L)
  check_seq_type(from, "from")

  # A missing center leaves the question open unless `x` rules out every
  # center. A dispersed sequence is centered on each mirrored pair of its
  # values, so the first known such center is the only candidate. The strings
  # `"NA"` and `"NaN"` are missing, as in `is_numeric_like()`:
  if (is.na(from) || is.na(is_numeric_like(from))) {
    x_num <- if (is.factor(x)) as.character(x) else x
    x_num <- as_numeric_seq(x_num)
    centers <- (x_num + rev(x_num)) / 2
    candidate <- centers[!is.na(centers)][1L]
    out <- if (is.na(candidate)) {
      !is_even(length(x)) &&
        !any(is.infinite(x_num)) &&
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

  # `isTRUE()` because `near(Inf, Inf)` is `NA`:
  if (!isTRUE(near_seq(x[index_central_x], from, tolerance, c(x, from)))) {
    return(FALSE)
  }

  dispersion_minus <- from - x[1L:(index_central_x - 1L)]
  dispersion_plus <- from + x[(index_central_x + 1L):length(x)]

  from_reconstructed <- (dispersion_plus - rev(dispersion_minus)) / 2

  all(near_seq(from, from_reconstructed, tolerance, c(x, from)))
}
