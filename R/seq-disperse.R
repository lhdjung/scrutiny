#' Sequences dispersed around a value
#'
#' @description `seq_disperse()` creates a sequence of values around `from`,
#'   a given number of steps up and down, where a step is one unit of the last
#'   decimal place: `7.93` is surrounded by `7.91`, `7.92`, `7.94`, `7.95`, and
#'   so on.
#'
#'   `seq_disperse_df()` returns the sequence as a data frame column, with
#'   further columns added as in [`tibble::tibble()`]. Its arguments are those
#'   of `seq_disperse()` with a leading dot.
#'
#' @param from,.from Numeric, or a string coercible to numeric. The value to
#'   disperse from.
#' @param by,.by Numeric. Step size. Default is `NULL`, in which case it is one
#'   unit of the last decimal place of `from`. A number does not keep trailing
#'   zeros, so `5.30` steps by `0.1`: give `by = 0.01`, or `from` as the string
#'   `"5.30"`.
#' @param dispersion,.dispersion Numeric. Whole numbers of steps up and down
#'   from `from`. Default is `1:5`.
#' @param offset_from,.offset_from Integer. Moves the starting point by that
#'   many steps. Default is `0`.
#' @param out_min,.out_min,out_max,.out_max Limits on the values, including
#'   `from` itself. `out_min` defaults to `"auto"`, one step above zero, which
#'   suits a count but not a quantity that can reach zero or be negative; use
#'   `NULL` for an unbounded quantity such as a mean, or `0` for one bounded
#'   at zero such as a standard deviation. `out_max` defaults to `NULL`, no
#'   maximum. (The `*_map_seq()` functions choose limits per variable; see
#'   `.var_bounds` in [`function_map_seq()`].)
#' @param include_reported,.include_reported Logical. Should `from` itself be
#'   part of the sequence? Default is `TRUE`.
#' @param track_diff_var,.track_diff_var Logical. Should the number of steps
#'   from `from` be returned as well? Default is `FALSE`. If `TRUE`,
#'   `seq_disperse()` returns a list of the sequence and the steps, and
#'   `seq_disperse_df()` adds a `diff_var` column.
#' @param ... Further columns, added as in [`tibble::tibble()`]. Only in
#'   `seq_disperse_df()`.
#'
#' @details Unlike [`base::seq()`], these functions need not return a regular
#'   sequence: `dispersion` can be any numeric vector.
#'
#' @return
#'   - `seq_disperse()` returns a numeric vector, which is integer if `from`
#'   is an integer and the steps are whole numbers. With `track_diff_var =
#'   TRUE`, it returns a list of that vector and an integer vector of steps.
#'   - `seq_disperse_df()` returns a tibble with the sequence in the `x`
#'   column, the steps in `diff_var` if requested, and any columns added via
#'   the dots.
#'
#' @export
#'
#' @seealso [`disperse()`], which `seq_disperse()` combines aspects of with
#'   [`base::seq()`].
#'
#' @examples
#' seq_disperse(from = 4.02)
#'
#' # A number loses trailing zeros, so state the step size:
#' seq_disperse(from = 5.30, by = 0.01)
#'
#' # More steps:
#' seq_disperse(from = 4.02, dispersion = 1:10)
#'
#' # Irregular steps:
#' seq_disperse(from = 4.02, dispersion = c(2, 3, 7))
#'
#' # The data frame variant takes further columns:
#' seq_disperse_df(.from = 4.02, n = 45)

seq_disperse <- function(
  from,
  by = NULL,
  dispersion = 1:5,
  offset_from = 0L,
  out_min = "auto",
  out_max = NULL,
  include_reported = TRUE,
  track_diff_var = FALSE
) {
  # Checks ---

  check_length(from, 1L)

  # A factor is its label, not its code:
  if (is.factor(from)) {
    from <- as.character(from)
  }
  check_type(dispersion, c("double", "integer"))
  if (anyNA(dispersion)) {
    abort_in_export("`dispersion` must not contain missing values.")
  }
  check_non_negative(dispersion)

  # A missing, infinite, or non-numeric `from` has no decimal level to disperse
  # on:
  from_num <- suppressWarnings(as.numeric(from))
  if (!is.finite(from_num)) {
    abort_in_export(
      "`from` must be a finite number, or a string coercible to one.",
      "x" = "It is {wrap_in_quotes_or_backticks(from)}."
    )
  }

  # A step of 0 is `from` itself, twice over. Whether `from` is included is up
  # to `include_reported`, and a duplicate would be counted twice by
  # `audit_seq()`:
  dispersion <- dispersion[dispersion != 0]

  # A fractional step would land off the decimal level of `from`. `disperse()`
  # rejects it for the same reason:
  if (!all(is_whole_number(dispersion))) {
    offenders <- dispersion[!is_whole_number(dispersion)]
    abort_in_export(
      "`dispersion` must be whole numbers.",
      "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
      {offenders}.",
      "i" = "Each value is a number of steps up and down from `from`, on \\
      the level of its last decimal place."
    )
  }

  # Main part ---

  if (is.null(by)) {
    digits <- decimal_places_scalar(from)
    by <- 1 / (10^digits)
  } else {
    check_length(by, 1L)
    check_type(by, c("integer", "double"))
    if (is.na(by) || by <= 0) {
      abort_in_export(
        "`by` must be a positive number.",
        "x" = "It is {by}."
      )
    }
    digits <- decimal_places_scalar(by)
  }

  check_length(offset_from, 1L)
  check_type(offset_from, c("integer", "double"))
  if (is.na(offset_from) || !is_whole_number(offset_from)) {
    abort_in_export(
      "`offset_from` must be a whole number.",
      "x" = "It is {offset_from}.",
      "i" = "It is a number of steps of `by`."
    )
  }
  if (!isTRUE(include_reported) && !isFALSE(include_reported)) {
    abort_in_export(
      "`include_reported` must be `TRUE` or `FALSE`.",
      "x" = "It is {wrap_in_quotes_or_backticks(include_reported)}."
    )
  }

  disp_minus <- dispersion * by
  disp_plus <- disp_minus

  # Floating-point arithmetic leaves the decimal level: `3.14 - (305 * 0.01)`
  # is `0.0899999999999999`. Every derived value is therefore rounded back to
  # `digits_out` before it is compared to the limits or returned. It is counted
  # before `from` becomes a number, which would drop the trailing zero of a
  # string like `"1.50"`:
  digits_out <- max(digits, decimal_places_scalar(from))

  from_is_integer <- is.integer(from)
  from <- as.numeric(from)

  # A string limit would be compared as a string, so `out_max = "10"` would
  # rule out `9`:
  check_limit <- function(limit, name, auto = FALSE) {
    limit_num <- suppressWarnings(as.numeric(limit))
    if (length(limit) != 1L || is.na(limit_num)) {
      msg_auto <- if (auto) ", \"auto\"," else ""
      msg_is <- if (length(limit) != 1L) {
        "It has length {length(limit)}."
      } else {
        "It is {wrap_in_quotes_or_backticks(limit)}."
      }
      abort_in_export(
        "`{name}` must be a single number{msg_auto} or `NULL`.",
        "x" = msg_is
      )
    }
    limit_num
  }

  if (identical(out_min, "auto")) {
    out_min <- by
  }
  if (!is.null(out_min)) {
    out_min <- check_limit(out_min, "out_min", auto = TRUE)
  }
  if (!is.null(out_max)) {
    out_max <- check_limit(out_max, "out_max")
  }

  # The offset moves the point the sequence is built around, so it has to be
  # applied before the values are compared to the limits:
  if (offset_from != 0L) {
    from <- round(from + (by * offset_from), digits_out)
  }

  # Both limits apply to both sides of the sequence: with `from = 30` and
  # `out_max = 25`, the steps down land on `29`, `28`, ..., which are above the
  # maximum just as the steps up are.
  is_within_range <- function(x) {
    ok <- rep(TRUE, length(x))
    if (!is.null(out_min)) {
      ok <- ok & x >= out_min
    }
    if (!is.null(out_max)) {
      ok <- ok & x <= out_max
    }
    ok
  }

  seq_lower <- round(from - disp_minus, digits_out)
  seq_upper <- round(from + disp_plus, digits_out)

  is_within_range_lower <- is_within_range(seq_lower)
  is_within_range_upper <- is_within_range(seq_upper)

  disp_minus_represent <- dispersion[is_within_range_lower]
  seq_lower <- seq_lower[is_within_range_lower]
  disp_plus_represent <- dispersion[is_within_range_upper]
  seq_upper <- seq_upper[is_within_range_upper]

  # The limits apply to `from` as well, even if `include_reported` is `TRUE`:
  include_reported <- include_reported && is_within_range(from)

  disp_zero <- if (include_reported) {
    from
  } else {
    NULL
  }

  out <- seq_lower |>
    rev() |>
    append(c(disp_zero, seq_upper))

  # An integer `from` dispersed by a fractional step must not be truncated back
  # to integer, so the sequence only becomes integer if that loses nothing:
  if (from_is_integer) {
    out <- as_integer_if_lossless(out)
  }

  if (!track_diff_var) {
    return(out)
  }

  disp_zero_represent <- if (include_reported) {
    0L
  } else {
    NULL
  }

  # The steps are integers, as documented, even if `dispersion` was doubles:
  list(
    out,
    c(-rev(disp_minus_represent), disp_zero_represent, disp_plus_represent) |>
      as_integer_if_lossless()
  )
}


#' @rdname seq_disperse
#' @export

seq_disperse_df <- function(
  .from,
  .by = NULL,
  ...,
  .dispersion = 1:5,
  .offset_from = 0L,
  .out_min = "auto",
  .out_max = NULL,
  .include_reported = TRUE,
  .track_diff_var = FALSE
) {
  out_basic_fun <- seq_disperse(
    from = .from,
    by = .by,
    dispersion = .dispersion,
    offset_from = .offset_from,
    out_min = .out_min,
    out_max = .out_max,
    include_reported = .include_reported,
    track_diff_var = .track_diff_var
  )

  if (.track_diff_var) {
    x <- out_basic_fun[[1L]]
    diff_var <- out_basic_fun[[2L]]
  } else {
    x <- out_basic_fun
    diff_var <- NULL
  }

  # Plain dots, not captured expressions, so that a caller's local variable is
  # found:
  tibble::tibble(x, diff_var, ...)
}


#' Helper function for dispersed sequence generation
#'
#' @description `seq_disperse_df_internal()` is a lightweight version of
#'   `seq_disperse_df()` for `function_map_seq_proto()`.
#'
#' @return A tibble with the columns `x` and `diff_var`.
#'
#' @noRd

seq_disperse_df_internal <- function(
  from,
  by = NULL,
  dispersion = 1:5,
  offset_from = 0L,
  out_min = "auto",
  out_max = NULL,
  include_reported = TRUE,
  track_diff_var = TRUE
) {
  from |>
    seq_disperse(
      by = by,
      dispersion = dispersion,
      offset_from = offset_from,
      out_min = out_min,
      out_max = out_max,
      include_reported = include_reported,
      track_diff_var = track_diff_var
    ) |>
    tibble::as_tibble(.name_repair = function(x) c("x", "diff_var"))
}
