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
#'   unit of the last decimal place of `from`.
#' @param dispersion,.dispersion Numeric. The steps up and down from `from`.
#'   Default is `1:5`.
#' @param offset_from,.offset_from Integer. Moves the starting point by that
#'   many steps. Default is `0`.
#' @param out_min,.out_min,out_max,.out_max Limits on the values, including
#'   `from` itself. `out_min` defaults to `"auto"`, one step above zero, which
#'   suits a count but not a quantity that can reach zero or be negative; use
#'   `NULL` for an unbounded quantity such as a mean, or `0` for one bounded
#'   at zero such as a standard deviation. `out_max` defaults to `NULL`, no
#'   maximum. (The `*_map_seq()` functions choose limits per variable; see
#'   `.var_bounds` in [`function_map_seq()`].)
#' @param string_output,.string_output Logical or `"auto"`. If `TRUE` (the
#'   default), the output is a string vector padded with trailing zeros to the
#'   decimal places of `from`. `"auto"` means `TRUE` if `from` is a string.
#' @param include_reported,.include_reported Logical. Should `from` itself be
#'   part of the sequence? Default is `TRUE`.
#' @param track_diff_var,.track_diff_var Logical. Should the number of steps
#'   from `from` be returned as well? Default is `FALSE`. If `TRUE`,
#'   `seq_disperse()` returns a list of the sequence and the steps, and
#'   `seq_disperse_df()` adds a `diff_var` column.
#' @param track_var_change,.track_var_change `r lifecycle::badge("deprecated")`
#'   Renamed to `track_diff_var` / `.track_diff_var`.
#' @param ... Further columns, added as in [`tibble::tibble()`]. Only in
#'   `seq_disperse_df()`.
#'
#' @details Unlike [`seq_endpoint()`] and friends, these functions do not wrap
#'   [`base::seq()`], and their output need not be regular: `dispersion` can
#'   be any numeric vector.
#'
#' @return
#'   - `seq_disperse()` returns a string vector by default and a numeric
#'   vector with `string_output = FALSE`. With `track_diff_var = TRUE`, it
#'   returns a list of that vector and an integer vector of steps.
#'   - `seq_disperse_df()` returns a tibble with the sequence in the `x`
#'   column, plus any columns added via the dots.
#'
#' @export
#'
#' @seealso [`seq_endpoint()`] and [`disperse()`], which `seq_disperse()`
#'   combines aspects of.
#'
#' @examples
#' seq_disperse(from = 4.02)
#'
#' # Numeric output:
#' seq_disperse(from = 4.02, string_output = FALSE)
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
  string_output = TRUE,
  include_reported = TRUE,
  track_diff_var = FALSE,
  track_var_change = deprecated()
) {
  # Checks ---

  # Any sequence can only proceed from a single number (for multiple numbers,
  # map the function). Also, the steps away from the number can't be negative:
  check_length(from, 1L)
  check_type(dispersion, c("double", "integer"))
  check_non_negative(dispersion)

  # A missing, infinite, or non-numeric `from` has no decimal level to disperse
  # on. It used to be blamed on `out_min`, or to fail in `restore_zeros()`:
  from_num <- suppressWarnings(as.numeric(from))
  if (!is.finite(from_num)) {
    cli::cli_abort(c(
      "`from` must be a finite number, or a string coercible to one.",
      "x" = "It is {wrong_spec_string(from)}."
    ))
  }

  # Each value in `dispersion` is a number of steps taken both up and down from
  # `from`, so a step of 0 is `from` itself -- twice over, once in each
  # direction. Whether `from` belongs in its own sequence is what
  # `include_reported` decides, so a zero step used to add it two more times:
  # `seq_disperse(4.02, dispersion = 0)` was `c("4.02", "4.02", "4.02")`, and
  # `grim_map_seq(dispersion = c(0, 1, 2))` returned the reported case twice,
  # which `audit_seq()` then counted twice:
  dispersion <- dispersion[dispersion != 0]

  # Each value is a number of steps, so a fractional one lands off the decimal
  # level that the output is padded to: `dispersion = 1.5` around `4` returned
  # `"2"`, `"4"`, and `"6"`, next to steps of `-1.5` and `1.5`. `disperse()`
  # rejects it for the same reason:
  if (!all(is_whole_number(dispersion))) {
    offenders <- dispersion[!is_whole_number(dispersion)]
    cli::cli_abort(c(
      "`dispersion` must be whole numbers.",
      "x" = "It has {length(offenders)} value{?s} that {?is/are} not: \\
      {offenders}.",
      "i" = "Each value is a number of steps up and down from `from`, on \\
      the level of its last decimal place."
    ))
  }

  if (!missing(track_var_change)) {
    lifecycle::deprecate_warn(
      when = "0.3.1",
      what = "seq_disperse(track_var_change)",
      details = "It was renamed to `track_diff_var`. \\
      If `track_var_change` is still specified, track_diff_var \\
      takes on its value."
    )
    track_diff_var <- track_var_change
  }

  # Main part ---

  # If the step size by which the sequence progresses (`by`) was not manually
  # chosen as in `seq()`, it is determined by the number of decimal places in
  # `from`:
  if (is.null(by)) {
    digits <- decimal_places_scalar(from)
    by <- 1 / (10^digits)
  } else {
    check_length(by, 1L)
    check_type(by, c("integer", "double"))
    digits <- decimal_places_scalar(by)
  }

  disp_minus <- dispersion * by
  disp_plus <- disp_minus

  # The sequence is meant to proceed on the decimal level of `by` (or of `from`,
  # if `by` was specified with fewer decimal places). Floating-point arithmetic
  # does not respect that level: with `from` at `3.14` and `dispersion` going up
  # to `305`, `from - (305 * 0.01)` is `0.0899999999999999`, not `0.09`. Every
  # value derived from `from`, `by`, and `dispersion` is therefore rounded back
  # to `digits_out` before it is compared to the limits or returned. It is
  # counted before `from` becomes a number, which would drop the trailing zero
  # of a string like `"1.50"`:
  digits_out <- max(digits, decimal_places_scalar(from))

  from_orig_type <- typeof(from)
  from <- as.numeric(from)

  # The limits are compared to numbers, so they have to be numbers themselves. A
  # string was compared as a string, so `out_max = "10"` ruled out `9` (it sorts
  # after `"10"`), and a missing value failed in `if ()`:
  check_limit <- function(limit, name, auto = FALSE) {
    limit_num <- suppressWarnings(as.numeric(limit))
    if (length(limit) != 1L || is.na(limit_num)) {
      msg_auto <- if (auto) ", \"auto\"," else ""
      msg_is <- if (length(limit) != 1L) {
        "It has length {length(limit)}."
      } else {
        "It is {wrong_spec_string(limit)}."
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

  # Drop the values that fall outside of the range specified by `out_min` and
  # `out_max`, along with the `dispersion` steps that led to them. Both limits
  # apply to both sides of the sequence: with `from = 30` and `out_max = 25`,
  # the steps down from `from` land on `29`, `28`, ..., which are above the
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

  # The limits apply to `from` as well: a reported value outside of them is
  # left out even if `include_reported` is `TRUE`, or the output would break
  # the very limits it was asked to keep:
  include_reported <- include_reported && is_within_range(from)

  disp_zero <- if (include_reported) {
    from
  } else {
    NULL
  }

  # Create sequences that are dispersed upward and downward, starting at `from`.
  # If this very value is meant to be included, it is positioned in between:
  out <- seq_lower |>
    rev() |>
    append(c(disp_zero, seq_upper))

  # Following user preferences, do or don't convert the output to string.
  # However, the default (`string_output == "auto"`) is to decide this by the
  # original type of `from`. Also, restore trailing zeros to the same number of
  # decimal places that also determined the unit of increments at the start of
  # the function:
  out <- manage_string_output_seq(
    out = out,
    from = methods::as(from, from_orig_type),
    string_output = string_output,
    digits = digits_out
  )

  # All the rest is only for creating and appending a sequence of dispersion
  # steps, so if this is not desired, the `out` vector is returned right now:
  if (!track_diff_var) {
    return(out)
  }

  # The complete vector of dispersion steps -- negative and positive -- includes
  # the midpoint at zero to represent `from` if and only if chosen by the user:
  disp_zero_represent <- if (include_reported) {
    0L
  } else {
    NULL
  }

  # Collect the sequence dispersed around `from` and the sequence of dispersion
  # steps in a list:
  list(
    out,
    c(-rev(disp_minus_represent), disp_zero_represent, disp_plus_represent)
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
  .string_output = TRUE,
  .include_reported = TRUE,
  .track_diff_var = FALSE,
  .track_var_change = FALSE
) {
  if (!missing(.track_var_change)) {
    lifecycle::deprecate_warn(
      when = "0.3.1",
      what = "seq_disperse_df(.track_var_change)",
      details = "It was renamed to `.track_diff_var`. \\
      If `.track_var_change` is still specified, .track_diff_var \\
      takes on its value."
    )
    .track_diff_var <- .track_var_change
  }

  out_basic_fun <- seq_disperse(
    from = .from,
    by = .by,
    dispersion = .dispersion,
    offset_from = .offset_from,
    out_min = .out_min,
    out_max = .out_max,
    string_output = .string_output,
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

  # Passing the dots on as they are, rather than as captured expressions,
  # evaluates them where the caller wrote them, so a local variable is found:
  tibble::tibble(x, diff_var, ...)
}


#' Helper function for dispersed sequence generation
#'
#' @description `seq_disperse_df_internal()` is a lightweight version of
#'   `seq_disperse_df()`. It's used as an internal helper for
#'   `function_map_seq_proto()`, which in turn powers `function_map_seq()`,
#'   which ultimately produces `grim_map_seq()` and other sequence mappers.
#'
#' @return A tibble (data frame).
#'
#' @noRd

seq_disperse_df_internal <- function(
  from,
  by = NULL,
  dispersion = 1:5,
  offset_from = 0L,
  out_min = "auto",
  out_max = NULL,
  string_output = TRUE,
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
      string_output = string_output,
      include_reported = include_reported,
      track_diff_var = track_diff_var
    ) |>
    tibble::as_tibble(.name_repair = function(x) c("x", "diff_var"))
}
