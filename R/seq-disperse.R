
#' Sequence generation with dispersion at decimal level
#'
#' @description `seq_disperse()` creates a sequence around a given number. It
#'   goes a specified number of steps up and down from it. Step size depends on
#'   the number's decimal places. For example, `7.93` will be surrounded by
#'   values like `7.91`, `7.92`, and `7.94`, `7.95`, etc.
#'
#'   `seq_disperse_df()` is a variant that creates a data frame. Further columns
#'   can be added as in `tibble::tibble()`. Regular arguments are the same as in
#'   `seq_disperse()`, but with a dot before each.
#'
#' @param from,.from Numeric (or string coercible to numeric). Starting point of
#'   the sequence.
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
#' @param string_output,.string_output Boolean or string. If `TRUE` (the
#'   default), the output is a string vector. Decimal places are then padded
#'   with zeros to match `from`'s number of decimal places. `"auto"` works like
#'   `TRUE` if and only if `from` (`.from`) is a string.
#' @param include_reported,.include_reported Boolean. Should `from` (`.from`)
#'   itself be part of the sequence built around it? Default is `TRUE` for the
#'   sake of continuity, but this can be misleading if the focus is on the
#'   dispersed values, as opposed to the input.
#' @param track_var_change,.track_var_change Boolean. In `seq_disperse()`,
#'   ignore this argument. In `seq_disperse_df()`, default is `TRUE`, which
#'   creates the `"var_change"` output column.
#' @param ... Further columns, added as in `tibble::tibble()`. Only in
#'   `seq_disperse_df()`.
#'
#' @details Unlike `seq_endpoint()` and friends, the present functions don't
#'   necessarily return continuous or even regular sequences. The greater
#'   flexibility is due to the `dispersion` (`.dispersion`) argument, which
#'   takes any numeric vector. By default, however, the output sequence is
#'   regular and continuous.
#'
#'   Underlying this difference is the fact that `seq_disperse()` and
#'   `seq_disperse_df()` do not wrap around `base::seq()`, although they are
#'   otherwise similar to `seq_endpoint()` and friends.

#' @return
#'   - `seq_disperse()` returns a string vector by default
#'   (`string_output = TRUE`) and a numeric otherwise.
#'   - `seq_disperse_df()` returns a tibble (data frame). The sequence is stored
#'   in the `x` column. `x` is string by default (`.string_output = TRUE`),
#'   numeric otherwise. Other columns might have been added via the dots
#'   (`...`).

#' @export
#'
#' @seealso Conceptually, `seq_disperse()` is a blend of two function families:
#'   those around `seq_endpoint()` and those around `disperse()`. The present
#'   functions were originally conceived for `seq_disperse_df()` to be a helper
#'   within the `function_map_seq()` implementation.
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



seq_disperse <- function(from, dispersion = 1:5, offset_from = 0,
                         out_min = "auto", out_max = NULL,
                         string_output = TRUE, include_reported = TRUE,
                         track_var_change = FALSE) {

  # Checks ---

  # Any sequence can only proceed from a single number (for multiple numbers,
  # map the function). Also, the steps away from the number can't be negative:
  check_length(from, 1)
  check_non_negative(dispersion)


  # Main part ---

  digits <- decimal_places_scalar(from)
  by <- 1 / (10 ^ digits)
  dispersion <- dispersion * by

  disp_minus <- dispersion
  disp_plus  <- dispersion

  from_orig <- from
  from <- as.numeric(from)

  # Filter the `dispersion` vector from values that fall outside of the range
  # specified by `out_min` at the lower end...
  if (!is.null(out_min)) {
    if (length(out_min) > 1) {
      cli::cli_abort(c(
        "`out_min` has length {length(out_min)}",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    if (out_min == "auto") {
      out_min <- by
    }
    disp_minus <- disp_minus[(from - disp_minus) >= out_min]
  }

  # ...and `out_max` at the upper end:
  if (!is.null(out_max)) {
    if (length(out_max) > 1) {
      cli::cli_abort(c(
        "`out_max` has length {length(out_max)}",
        "x" = "It needs to have length 1 or to be `NULL`."
      ))
    }
    disp_plus <- disp_plus[(from + disp_plus) <= out_max]
  }


  if (offset_from != 0) {
    from <- from + (by * offset_from)
  }

  from_minus <- from - disp_minus
  from_plus  <- from + disp_plus

  if (include_reported) {
    out <- append(rev(from_minus), c(from, from_plus))
  } else {
    out <- append(rev(from_minus), from_plus)
  }

  # Somewhat hackish way of conveying to `manage_string_output_seq()` whether or
  # not `from` was specified as a string:
  from <- methods::as(from, typeof(from_orig))

  # Following user preferences, do or don't convert the output to string,
  # restoring trailing zeros to the same number of decimal places that also
  # determined the unit of increments at the start of the function:
  out <- manage_string_output_seq(
    out = out, from = from, string_output = string_output, digits = digits
  )

  if (track_var_change) {
    var_change <- as.numeric(out) - as.numeric(from)
    out <- list(out, var_change)
  }

  return(out)
}



#' @rdname seq_disperse
#' @export

seq_disperse_df <- function(.from, ..., .dispersion = 1:5, .offset_from = 0,
                            .out_min = "auto", .out_max = NULL,
                            .string_output = TRUE, .include_reported = TRUE,
                            .track_var_change = FALSE) {

  further_cols <- rlang::enexprs(...)

  out_basic_fun <- seq_disperse(
    from = .from, dispersion = .dispersion, offset_from = .offset_from,
    out_min = .out_min, out_max = .out_max, string_output = .string_output,
    include_reported = .include_reported, track_var_change = .track_var_change
  )

  if (.track_var_change) {
    x <- out_basic_fun[[1]]
    var_change <- out_basic_fun[[2]]
  } else {
    x <- out_basic_fun
    var_change <- NULL
  }

  if (length(further_cols) > 0) {
    out <- tibble::tibble(x, var_change, !!!further_cols)
  } else {
    out <- tibble::tibble(x, var_change)
  }

  return(out)
}


