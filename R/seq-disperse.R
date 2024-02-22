
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
#' @param out_min,.out_min,out_max,.out_max If specified, output will be
#'   restricted so that it's not below `out_min` or above `out_max`. Defaults
#'   are `"auto"` for `out_min`, i.e., a minimum of one decimal unit above zero;
#'   and `NULL` for `out_max`, i.e., no maximum.
#' @param string_output,.string_output Logical or string. If `TRUE` (the
#'   default), the output is a string vector. Decimal places are then padded
#'   with zeros to match `from`'s number of decimal places. `"auto"` works like
#'   `TRUE` if and only if `from` (`.from`) is a string.
#' @param include_reported,.include_reported Logical. Should `from` (`.from`)
#'   itself be part of the sequence built around it? Default is `TRUE` for the
#'   sake of continuity, but this can be misleading if the focus is on the
#'   dispersed values, as opposed to the input.
#' @param track_diff_var,.track_diff_var Logical. In `seq_disperse()`, ignore
#'   this argument. In `seq_disperse_df()`, default is `TRUE`, which creates the
#'   `"diff_var"` output column.
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
#'   (`string_output = TRUE`) and a numeric vector otherwise.
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


seq_disperse <- function(from, by = NULL, dispersion = 1:5, offset_from = 0L,
                         out_min = "auto", out_max = NULL,
                         string_output = TRUE, include_reported = TRUE,
                         track_diff_var = FALSE,
                         track_var_change = deprecated()) {

  # Checks ---

  # Any sequence can only proceed from a single number (for multiple numbers,
  # map the function). Also, the steps away from the number can't be negative:
  check_length(from, 1L)
  check_non_negative(dispersion)

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
    by <- 1 / (10 ^ digits)
  } else {
    check_length(by, 1L)
    check_type(by, c("integer", "double"))
    digits <- decimal_places_scalar(by)
  }

  disp_minus <- dispersion * by
  disp_plus  <- disp_minus

  from_orig_type <- typeof(from)
  from <- as.numeric(from)

  # Filter the `dispersion` vector -- and the `disp_minus` vector derived from
  # it -- from values that fall outside of the range specified by `out_min` at
  # the lower end:
  if (is.null(out_min)) {
    disp_minus_represent <- dispersion
  } else {
    if (length(out_min) > 1L) {
      cli::cli_abort(c(
        "!" = "`out_min` must have length 1 or to be `NULL`.",
        "x" = "It has length {length(out_min)}."
      ))
    }
    if (out_min == "auto") {
      out_min <- by
    }
    is_within_range_lower <- (from - disp_minus) >= out_min
    disp_minus_represent <- dispersion[is_within_range_lower]
    disp_minus <- disp_minus[is_within_range_lower]
  }

  # Do the same but for `disp_plus` and `out_max` at the upper end:
  if (is.null(out_max)) {
    disp_plus_represent <- dispersion
  } else {
    if (length(out_max) > 1L) {
      cli::cli_abort(c(
        "!" = "`out_max` must have length 1 or to be `NULL`.",
        "x" = "It has length {length(out_max)}."
      ))
    }
    is_within_range_upper <- (from + disp_plus) <= out_max
    disp_plus_represent <- dispersion[is_within_range_upper]
    disp_plus <- disp_plus[is_within_range_upper]
  }

  if (offset_from != 0L) {
    from <- from + (by * offset_from)
  }

  disp_zero <- if (include_reported) {
    from
  } else {
    NULL
  }

  # Create sequences that are dispersed upward and downward, starting at `from`.
  # If this very value is meant to be included, it is positioned in between:
  out <- append(rev(from - disp_minus), c(disp_zero, from + disp_plus))

  # Following user preferences, do or don't convert the output to string.
  # However, the default (`string_output == "auto"`) is to decide this by the
  # original type of `from`. Also, restore trailing zeros to the same number of
  # decimal places that also determined the unit of increments at the start of
  # the function:
  out <- manage_string_output_seq(
    out = out, from = methods::as(from, from_orig_type),
    string_output = string_output, digits = digits
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

seq_disperse_df <- function(.from, .by = NULL, ...,
                            .dispersion = 1:5, .offset_from = 0L,
                            .out_min = "auto", .out_max = NULL,
                            .string_output = TRUE, .include_reported = TRUE,
                            .track_diff_var = FALSE,
                            .track_var_change = FALSE) {

  further_cols <- rlang::enexprs(...)

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
    from = .from, by = .by, dispersion = .dispersion,
    offset_from = .offset_from, out_min = .out_min, out_max = .out_max,
    string_output = .string_output, include_reported = .include_reported,
    track_diff_var = .track_diff_var
  )

  if (.track_diff_var) {
    x <- out_basic_fun[[1L]]
    diff_var <- out_basic_fun[[2L]]
  } else {
    x <- out_basic_fun
    diff_var <- NULL
  }

  if (length(further_cols) > 0L) {
    tibble::tibble(x, diff_var, !!!further_cols)
  } else {
    tibble::tibble(x, diff_var)
  }
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

seq_disperse_df_internal <- function(.from, .by = NULL,
                                     .dispersion = 1:5, .offset_from = 0L,
                                     .out_min = "auto", .out_max = NULL,
                                     .string_output = TRUE,
                                     .include_reported = TRUE,
                                     .track_diff_var = TRUE) {

  tibble::as_tibble(
    seq_disperse(
      from = .from, by = .by, dispersion = .dispersion,
      offset_from = .offset_from, out_min = .out_min, out_max = .out_max,
      string_output = .string_output, include_reported = .include_reported,
      track_diff_var = .track_diff_var
    ),
    .name_repair = function(x) c("x", "diff_var")
  )
}

