#' Sequence generation with dispersion at decimal level
#'
#' @description `seq_disperse()` creates a sequence around a given number. It
#'   goes a specified number of steps up and down from it. Step size depends on
#'   the number's decimal places. For example, `7.93` will be surrounded by
#'   values like `7.91`, `7.92`, and `7.94`, `7.95`, etc.
#'
#'   `seq_disperse_df()` is a variant that creates a data frame. Further columns
#'   can be added as in [`tibble::tibble()`]. Regular arguments are the same as
#'   in `seq_disperse()`, but with a dot before each.
#'
#' @param from,.from Numeric (or string coercible to numeric). Starting point of
#'   the sequence.
#' @param by,.by Numeric. Step size of the sequence. If not set, inferred
#'   automatically. Default is `NULL`.
#' @param dispersion,.dispersion Numeric. Vector that determines the steps up
#'   and down, starting at `from` (or `.from`, respectively) and proceeding on
#'   the level of its last decimal place. Default is `1:5`, i.e., five steps up
#'   and down.
#' @param offset_from,.offset_from Integer. If set to a non-zero number, the
#'   starting point will be offset by that many units on the level of the last
#'   decimal digit. Default is `0`.
#' @param out_min,.out_min,out_max,.out_max If specified, the dispersed values
#'   will be restricted so that they're not below `out_min` or above `out_max`.
#'   This includes `from` itself, even if `include_reported` is `TRUE`. Defaults
#'   are `"auto"` for `out_min`, i.e., a minimum of one decimal unit above zero;
#'   and `NULL` for `out_max`, i.e., no maximum.
#'
#'   The `"auto"` default suits a count, which cannot go below one unit, and it
#'   is the wrong default for anything that can reach zero or go negative. A
#'   sequence around a negative `from` loses its whole lower half to it, and `0`
#'   is out of reach whatever `from` is. Pass `out_min = NULL` for an unbounded
#'   quantity such as a mean, or `out_min = 0` for one that is bounded at zero,
#'   such as a standard deviation. The `*_map_seq()` functions choose per
#'   variable and need none of this; see `.var_bounds` in [`function_map_seq()`].
#' @param string_output,.string_output Logical or string. If `TRUE` (the
#'   default), the output is a string vector. Decimal places are then padded
#'   with zeros to match `from`'s number of decimal places. `"auto"` works like
#'   `TRUE` if and only if `from` (`.from`) is a string.
#' @param include_reported,.include_reported Logical. Should `from` (`.from`)
#'   itself be part of the sequence built around it? Default is `TRUE` for the
#'   sake of continuity, but this can be misleading if the focus is on the
#'   dispersed values, as opposed to the input.
#' @param track_diff_var,.track_diff_var Logical. Should the number of steps
#'   from `from` be returned as well? Default is `FALSE`. If `TRUE`,
#'   `seq_disperse()` returns a list of the sequence and the steps, and
#'   `seq_disperse_df()` adds them as a `diff_var` column.
#' @param track_var_change,.track_var_change `r lifecycle::badge("deprecated")`
#'   Renamed to `track_diff_var` / `.track_diff_var`.
#' @param ... Further columns, added as in [`tibble::tibble()`]. Only in
#'   `seq_disperse_df()`.
#'
#' @details Unlike [`seq_endpoint()`] and friends, the present functions don't
#'   necessarily return continuous or even regular sequences. The greater
#'   flexibility is due to the `dispersion` (`.dispersion`) argument, which
#'   takes any numeric vector. By default, however, the output sequence is
#'   regular and continuous.
#'
#'   Underlying this difference is the fact that `seq_disperse()` and
#'   `seq_disperse_df()` do not wrap around [`base::seq()`], although they are
#'   otherwise similar to [`seq_endpoint()`] and friends.

#' @return
#'   - `seq_disperse()` returns a string vector by default
#'   (`string_output = TRUE`) and a numeric vector otherwise. With
#'   `track_diff_var = TRUE`, it returns an unnamed list of that vector and an
#'   integer vector of the steps from `from`.
#'   - `seq_disperse_df()` returns a tibble (data frame). The sequence is stored
#'   in the `x` column. `x` is string by default (`.string_output = TRUE`),
#'   numeric otherwise. Other columns might have been added via the dots
#'   (`...`).

#' @export
#'
#' @seealso Conceptually, `seq_disperse()` is a blend of two function families:
#'   those around [`seq_endpoint()`] and those around [`disperse()`]. The
#'   present functions were originally conceived for `seq_disperse_df()` to be a
#'   helper within the [`function_map_seq()`] implementation.
#'
#' @examples
#' # Basic usage:
#' seq_disperse(from = 4.02)
#'
#' # If trailing zeros don't matter,
#' # the output can be numeric:
#' seq_disperse(from = 4.02, string_output = FALSE)
#'
#' # Control steps up and down with
#' # `dispersion` (default is `1:5`):
#' seq_disperse(from = 4.02, dispersion = 1:10)
#'
#' # Sequences might be discontinuous...
#' disp1 <- seq(from = 2, to = 10, by = 2)
#' seq_disperse(from = 4.02, dispersion = disp1)
#'
#' # ...or even irregular:
#' disp2 <- c(2, 3, 7)
#' seq_disperse(from = 4.02, dispersion = disp2)
#'
#' # The data fame variant supports further
#' # columns added as in `tibble::tibble()`:
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
