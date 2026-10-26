#' Vary hypothetical group sizes
#'
#' @description If a study reports a total sample size but no group sizes,
#'   these functions generate pairs of hypothetical group sizes that add up to
#'   the total:
#'
#'   - `disperse()` takes half of an even total and goes up and down from it.
#'   - `disperse2()` takes the two consecutive numbers around half of an odd
#'   total, and goes down from the lower and up from the upper.
#'   - `disperse_total()` takes the total itself and calls one of the two,
#'   depending on whether it is even or odd.
#'
#'   They are the basis of [`grim_map_total_n()`] and the other functions made
#'   by [`function_map_total_n()`].
#'
#' @param n Numeric. In `disperse()`, half of an even total. In `disperse2()`,
#'   the two consecutive numbers closest to half of an odd total, such as
#'   `c(25, 26)` for 51. In `disperse_total()`, the total itself.
#' @param dispersion Numeric. Steps up and down from `n` (or, in
#'   `disperse_total()`, from half of `n`). Default is `0:5`.
#' @param n_min Numeric. Minimal group size. Default is `1L`.
#' @param n_max Numeric. Maximal group size. Default is `NULL`, no maximum.
#' @param constant Optionally, a length-2 vector, or a list of such vectors
#'   (which includes a data frame with two rows), to repeat alongside each pair
#'   of group sizes. Default is `NULL`.
#'
#' @details A pair with a group size below `n_min` or above `n_max` is
#'   dropped. Columns from `constant` are named after a named list's elements,
#'   or `"constant1"`, `"constant2"`, etc. for an unnamed one; a single pair is
#'   `"constant"`.
#'
#' @return A tibble with these columns:
#' - `n`: the group sizes. Each pair of consecutive rows adds up to the total.
#' - `n_change`: the number of steps from the input `n`, negative for the lower
#'   group size of each pair.
#'
#' @seealso [`function_map_total_n()`] and [`grim_map_total_n()`].
#'
#' @include utils.R
#'
#' @export
#'
#' @references Bauer, P. J., & Francis, G. (2021). Expression of Concern: Is It
#'   Light or Dark? Recalling Moral Behavior Changes Perception of Brightness.
#'   *Psychological Science*, 32(12), 2042–2043.
#'   https://journals.sagepub.com/doi/10.1177/09567976211058727
#'
#' @examples
#' # For a total of 40, `n` is 20:
#' disperse(n = 20)
#'
#' disperse(n = 20, dispersion = c(3, 6, 10))
#'
#' # For an odd total, `disperse2()` takes two group sizes:
#' disperse2(n = c(25, 26))
#'
#' # `disperse_total()` takes the total itself:
#' disperse_total(n = 40)
#' disperse_total(n = 51)
#'
#' # Values that repeat alongside the pairs:
#' disperse_total(37, constant = c("5.24", "3.80"))
#'
#' df_constant <- tibble::tibble(
#'   name = c("Paul", "Mathilda"), age = 27:28,
#'   registered = c(TRUE, FALSE)
#' )
#' disperse_total(37, constant = df_constant)

# Basic function for halves of even totals --------------------------------

disperse <- function(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL
) {
  # Checks ---

  check_length_disperse_n(n, "It must have length 1.")
  check_type(n, c("double", "integer"))
  if (is.na(n)) {
    abort_in_export("`n` must not be missing.")
  }
  check_type(dispersion, c("double", "integer"))
  if (anyNA(dispersion)) {
    abort_in_export("`dispersion` must not contain missing values.")
  }
  check_non_negative(dispersion)

  # `dispersion` counts whole units away from `n`, and `n` is a group size. A
  # fractional step has no meaning here, and it used to be accepted and then
  # quietly mangled: `n` moved by the fractional amount while `n_change` was
  # truncated toward zero by the `as.integer()` at the end of this function, so
  # `disperse(n = 10, dispersion = c(0.5, 1.5))` reported changes of 0 and 1 for
  # values of 9.5 and 8.5.
  if (!all(is_whole_number(dispersion))) {
    offenders <- dispersion[!is_whole_number(dispersion)]
    abort_in_export(
      "`dispersion` must be whole numbers.",
      "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
      {offenders}.",
      "i" = "Each value is a number of units by which the group sizes are \\
      moved apart, and a group has a whole number of members."
    )
  }

  # Main part ---

  # (Note: The checks below count towards to the main part because they may lead
  # to input transformations.)

  if (!is.null(n_min)) {
    check_limit_n(n_min, "n_min")
    dispersion <- dispersion[(n - dispersion) >= n_min]
  }

  if (!is.null(n_max)) {
    check_limit_n(n_max, "n_max")
    dispersion <- dispersion[(n + dispersion) <= n_max]
  }

  n_minus <- n - dispersion
  n_plus <- n + dispersion

  minus_plus_df <- tibble::tibble(n_minus, n_plus)

  out <- minus_plus_df |>
    tidyr::pivot_longer(
      cols = everything(),
      names_to = "n_change",
      values_to = "n"
    )

  n_change_num <- out$n - n
  out$n_change <- n_change_num

  if (!is_whole_number(n)) {
    digits <- decimal_places_scalar(n)
    out$n_change <- round(out$n_change, digits)
  }

  out <- out |>
    rev() |>
    add_class("scrutiny_disperse")

  if (!is.null(constant)) {
    repeat_constant <- function(constant, out, list_input = FALSE) {
      if (list_input) {
        constant_list_element <- constant
        check_length(constant_list_element, 2L, allow_null = TRUE)
      } else {
        check_length(constant, 2L, allow_null = TRUE)
      }
      rep(constant, times = nrow(out) / 2L)
    }

    if (is.list(constant)) {
      # Elements without a name, which may be all of them, are named
      # `constant1`, `constant2`, etc. A partly named list used to fail with
      # "attempt to use zero-length variable name":
      names_constant <- names(constant)
      if (is.null(names_constant)) {
        names_constant <- rep("", length(constant))
      }
      unnamed <- is.na(names_constant) | names_constant == ""
      names_constant[unnamed] <-
        paste0("constant", seq_along(constant))[unnamed]
      constant <- constant |>
        purrr::map(function(x) repeat_constant(x, out, list_input = TRUE)) |>
        tibble::as_tibble(.name_repair = function(x) names_constant)
    } else {
      constant <- repeat_constant(constant, out)
    }

    out <- out |>
      dplyr::mutate(constant, .after = "n_change")
  }

  # A sample size is a whole number, and the mappers that these tibbles feed
  # into store it as integer for that reason. The dispersion helpers returned
  # doubles, so `n` changed type on the way in and back out again -- and a
  # column that prints as `20` in one function and `20.0` in the next is the
  # kind of difference that only shows up in a comparison. `disperse2()` and
  # `disperse_total()` build on this function and inherit the coercion.
  #
  # It is conditional for the same reason as in the mappers: `as.integer()`
  # turns anything past `.Machine$integer.max` into `NA`. A half-integer `n`,
  # which `disperse2()` passes in, also keeps the column double -- although
  # `disperse2()` corrects the halves before returning.
  out$n <- as_integer_if_lossless(out$n)
  out$n_change <- as_integer_if_lossless(out$n_change)
  out
}


# Variant for halves of odd totals ----------------------------------------

#' @rdname disperse
#' @export

disperse2 <- function(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL
) {
  # Checks ---

  check_length(n, 2L)

  if (!dplyr::near(n[2L] - n[1L], 1)) {
    cli::cli_warn(c(
      "`n` was given as `{n[1L]}` and `{n[2L]}`.",
      "!" = "It should be two consecutive numbers.",
      ">" = "(The second value in `n` should be the first plus 1.)"
    ))
  }

  # Main part ---

  # Take the mean of the two `n` values and disperse from there. The limits
  # apply to the group sizes, which are half a unit further out than the
  # values dispersed from the midpoint; a fractional `n_min` used to let a pair
  # through that broke it:
  check_limit_n(n_min, "n_min")
  check_limit_n(n_max, "n_max")
  out <- disperse(
    n = mean(n),
    constant = constant,
    dispersion = dispersion,
    n_min = if (!is.null(n_min)) n_min + 0.5,
    n_max = if (!is.null(n_max)) n_max - 0.5
  )

  # Increase or decrease the dispersed values so that the lower values (odd
  # rows) decrease from the first of the two `n` values, the higher values (even
  # rows) increase from the second one, and both interleaved sequences proceed
  # by increments of 1. `rep_len()` also covers the case where `n_min` or
  # `n_max` left no pairs at all; the row locations used to be computed by
  # `seq(1, 0, by = 2)` there, which failed with "wrong sign in 'by' argument":
  out$n <- out$n + rep_len(c(-0.5, 0.5), nrow(out))

  # `disperse()` dispersed from a half-integer, so it left `n` as a double.
  # Adding and subtracting the halves above lands every value back on a whole
  # number, which is what the column should say it holds:
  out$n <- as_integer_if_lossless(out$n)

  # Return the resulting tibble:
  out
}


# Variant for totals; with even / odd splitting ---------------------------

#' @rdname disperse
#' @export

disperse_total <- function(
  n,
  dispersion = 0:5,
  n_min = 1L,
  n_max = NULL,
  constant = NULL
) {
  # Checks ---

  check_length_disperse_n(
    n,
    "It must have length 1; `n` is supposed \\
    to be a *single*, total sample size."
  )

  # A total sample size is a whole number. Anything else used to be split into
  # halves of halves without a word -- `disperse_total(7.5)` returned group
  # sizes of 3.25 and 4.25 -- because the even-or-odd branch below is only
  # exhaustive for whole numbers:
  if (is.na(n) || !is_whole_number(n) || n < 0) {
    abort_in_export(
      "`n` must be a non-negative whole number.",
      "x" = "It is {n}.",
      "i" = "It is a total sample size, to be split into two group sizes."
    )
  }

  # Main part ---

  n_half <- n / 2

  # Test if `n` is even, then call the appropriate function. If `n` is even,
  # call `disperse()`; if `n` is odd, call `disperse2()`:
  if (is_even(n)) {
    disperse(
      n = n_half,
      dispersion = dispersion,
      n_min = n_min,
      n_max = n_max,
      constant = constant
    )
  } else {
    # Determine the two whole numbers closest to half of the odd `n`:
    disperse2(
      n = c(n_half - 0.5, n_half + 0.5),
      dispersion = dispersion,
      n_min = n_min,
      n_max = n_max,
      constant = constant
    )
  }
}


# A missing limit used to pass the length check and turn every pair into `NA`.
# `disperse2()` checks the limits before shifting them, so that the message
# shows what the user gave:
check_limit_n <- function(limit, name) {
  if (
    !is.null(limit) &&
      (length(limit) != 1L || !is.numeric(limit) || is.na(limit))
  ) {
    abort_in_export(
      "`{name}` must be a single number or `NULL`.",
      "x" = "It is {wrap_in_quotes_or_backticks(limit)}."
    )
  }
}
