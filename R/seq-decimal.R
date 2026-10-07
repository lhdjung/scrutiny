# Shared tail of `seq_endpoint_df()` and `seq_distance_df()`: wrap the sequence
# in a tibble, adding whatever further columns the caller named in the dots. The
# attribute only passes messages between (1) these functions, (2) the testing
# function, and (3) `seq_test_ranking()`.
seq_df_out <- function(x, ...) {
  out <- tibble::tibble(x, ...)
  attr(out, "scrutiny") <- list(seq_df = TRUE)
  out
}


#' Sequences at the decimal level of their inputs
#'
#' @description These functions create sequences whose step size is one unit
#'   of the last decimal place of their inputs, such as `2.45`, `2.46`,
#'   `2.47`:
#'
#'   - `seq_endpoint()` goes from `from` to `to`, with the step size taken from
#'   the one with more decimal places.
#'   - `seq_distance()` goes from `from` for `length_out` values, with the step
#'   size taken from `from`.
#'
#'   `seq_endpoint_df()` and `seq_distance_df()` return the sequence as a data
#'   frame column, with further columns added as in [`tibble::tibble()`]. Their
#'   arguments are those of the vector functions with a leading dot.
#'
#' @details If `from` or `to` ends on zero, pass it as a string, since a
#'   numeric value loses trailing zeros. [`restore_zeros()`] formats values
#'   this way. The output is string by default for the same reason.
#'
#'   These functions wrap [`base::seq()`].
#'
#' @param from,.from Numeric, or a string coercible to numeric. Starting point.
#' @param to,.to Numeric, or a string coercible to numeric. Endpoint. Only in
#'   `seq_endpoint()` and `seq_endpoint_df()`.
#' @param by,.by Numeric. Step size. Only in `seq_distance()` and
#'   `seq_distance_df()`. Default is `NULL`, in which case it is inferred from
#'   `from`. If given, the output has as many decimal places as the one of
#'   `from` and `by` with more decimal places.
#' @param ... Further columns, added as in [`tibble::tibble()`]. Only in the
#'   `*_df()` functions.
#' @param length_out,.length_out Integer. Length of the sequence, at least 1.
#'   Default is `10`. Only in `seq_distance()` and `seq_distance_df()`.
#' @param dir,.dir Integer. `-1` for a descending sequence. Default is `1`.
#'   Only in `seq_distance()` and `seq_distance_df()`.
#' @param offset_from,.offset_from Integer. Moves the starting point by that
#'   many steps. Default is `0`.
#' @param offset_to,.offset_to Integer. Moves the endpoint by that many steps.
#'   Default is `0`. Only in `seq_endpoint()` and `seq_endpoint_df()`.
#' @param string_output,.string_output Logical or `"auto"`. If `TRUE` (the
#'   default), the output is a string vector padded with trailing zeros to the
#'   decimal places of `from` (or `to`). `"auto"` means `TRUE` if `from` is a
#'   string.
#'
#' @include decimal-places.R restore-zeros.R
#'
#' @return String by default, numeric with `string_output = FALSE`.
#'
#' @name seq-decimal
#'
#' @seealso [`seq_disperse()`] for sequences centered on a value.
#'
#' @export
#'
#' @examples
#' seq_endpoint(from = 4.7, to = 5)
#'
#' seq_distance(from = 0.93)
#' seq_distance(from = 0.93, length_out = 5)
#'
#' # Offsets:
#' seq_endpoint(from = 14.2, to = 15, offset_from = 4)
#' seq_endpoint(from = 9.5, to = 10, offset_to = 2)
#'
#' # Descending:
#' seq_distance(from = 20.03, dir = -1)
#'
#' # The data frame variants take further columns:
#' seq_endpoint_df(.from = 4.7, .to = 5, n = 20)
#' seq_distance_df(.from = 0.43, .length_out = 5, sd = 0.08)

seq_endpoint <- function(
  from,
  to,
  offset_from = 0L,
  offset_to = 0L,
  string_output = TRUE
) {
  # A number's neighborhood depends on how many decimal places the number has.
  # Therefore, in this function, the step size (`by`) can't be manually chosen
  # as in `seq()`. Instead, it's determined by whichever extreme (starting point
  # or endpoint) has more decimal places:
  digits <- max(decimal_places_scalar(from), decimal_places_scalar(to))
  by <- 1 / (10^digits)

  from_orig <- from
  to_orig <- to

  # After that, trailing zeros can safely be dropped because `from` and `to` are
  # only relevant in terms of their numeric values:
  from <- as.numeric(from)
  to <- as.numeric(to)

  # The starting point and/or the endpoint might be offset by some non-zero
  # number of incremental steps. First, the starting point...
  if (offset_from != 0L) {
    from <- round(from + (by * offset_from), digits)
  }

  # ...and then, the endpoint:
  if (offset_to != 0L) {
    to <- round(to + (by * offset_to), digits)
  }

  # If the endpoint is not greater than the starting point, the sequence will go
  # backward. This requires the step size to be negative:
  if (!to > from) {
    by <- -by
  }

  # Generate the sequence. Floating-point arithmetic does not respect the
  # decimal level of `by`: `seq(4.6, 0.1, by = -0.1)` has `0.799999999999999`
  # in it, which `restore_zeros()` then rejected, and which compares unequal
  # to `0.8`. Rounding back to that level fixes it, as in `seq_disperse()`:
  out <- from |>
    seq(to = to, by = by) |>
    suppressWarnings() |>
    round(digits)

  # Hackish way of conveying to `manage_string_output_seq()` whether or not
  # either of `from` and `to` was specified as a string, or else as a double:
  if (is.character(from_orig) || is.character(to_orig)) {
    from <- as.character(from)
  } else if (is.double(from_orig) || is.double(to_orig)) {
    from <- as.double(from)
  } else {
    from <- as.integer(from)
  }

  # Following user preferences, do or don't convert the output to string,
  # restoring trailing zeros to the same number of decimal places that also
  # determined the unit of increments at the start of the function:
  manage_string_output_seq(
    out = out,
    from = from,
    string_output = string_output,
    digits = digits
  )
}


#' @rdname seq-decimal
#' @export

seq_distance <- function(
  from,
  by = NULL,
  length_out = 10L,
  dir = 1,
  offset_from = 0L,
  string_output = TRUE
) {
  # If the step size by which the sequence progresses (`by`) was not manually
  # chosen as in `seq()`, it is determined by the number of decimal places in
  # `from`:
  if (is.null(by)) {
    digits <- decimal_places_scalar(from)
    by <- 1 / (10^digits)
  } else {
    check_length(by, 1L)
    check_type(by, c("integer", "double"))
    # The sequence proceeds from `from` in steps of `by`, so it has the decimal
    # places of either, whichever are more. Going by `by` alone,
    # `seq_distance(1.25, by = 1)` failed, and `"1.50"` lost its trailing zero:
    digits <- max(
      decimal_places_scalar(from),
      decimal_places_scalar(by)
    )
  }

  # A sequence has at least its starting point. Anything shorter used to reach
  # `seq()` as an endpoint on the wrong side of `from`:
  if (length_out < 1L) {
    cli::cli_abort(c(
      "`length_out` must be at least 1.",
      "x" = "It is {length_out}."
    ))
  }

  # Record if `from` was specified as string; relevant for `string_output`:
  from_orig <- from

  # After that, trailing zeros can safely be dropped because `from` is only
  # relevant in terms of its numeric value:
  from <- as.numeric(from)

  # The starting point might be offset by some non-zero number of incremental
  # steps (the default is 0, in which case this does nothing):
  if (offset_from != 0L) {
    from <- round(
      from + (by * offset_from),
      digits
    )
  }

  # The distance between the starting point and the end point follows from the
  # step size in conjunction with the desired output length (`length_out`).
  # However, the starting point is also part of the sequence, so it must be
  # compensated by subtracting 1 from the desired length:
  distance <- by * (length_out - 1L)

  # The endpoint of the sequence, `to`, is simply the starting point plus the
  # distance. However, that's just the default. If `dir` was set to `-1`, it
  # rather is the starting point *minus* the distance. Also, in that case, the
  # sign of the step size is reversed because the sequence is supposed to go
  # backward:
  if (dir == 1) {
    to <- from + distance
  } else if (dir == -1) {
    to <- from - distance
    by <- -by
  } else {
    cli::cli_abort(c(
      "!" = "`dir` must be either `1` or `-1`.",
      "x" = "It was given as `{dir}`."
    ))
  }

  # Generate the sequence, rounding back to the decimal level as in
  # `seq_endpoint()`:
  out <- from |>
    seq(to = to, by = by) |>
    suppressWarnings() |>
    round(digits)

  # Hackish way of conveying to `manage_string_output_seq()` whether or not
  # `from` was specified as a string:
  from <- from |>
    methods::as(typeof(from_orig))

  # Following user preferences, do or don't convert the output to string,
  # restoring trailing zeros to the same number of decimal places that also
  # determined the unit of increments at the start of the function:
  manage_string_output_seq(
    out = out,
    from = from,
    string_output = string_output,
    digits = digits
  )
}


#' @rdname seq-decimal
#' @export

seq_endpoint_df <- function(
  .from,
  .to,
  ...,
  .offset_from = 0L,
  .offset_to = 0L,
  .string_output = TRUE
) {
  # Call the basic function to generate the sequence:
  x <- seq_endpoint(
    from = .from,
    to = .to,
    offset_from = .offset_from,
    offset_to = .offset_to,
    string_output = .string_output
  )

  seq_df_out(x, ...)
}


#' @rdname seq-decimal
#' @export

seq_distance_df <- function(
  .from,
  .by = NULL,
  ...,
  .length_out = 10L,
  .dir = 1,
  .offset_from = 0L,
  .string_output = TRUE
) {
  # Call the basic function to generate the sequence:
  x <- seq_distance(
    from = .from,
    by = .by,
    length_out = .length_out,
    dir = .dir,
    offset_from = .offset_from,
    string_output = .string_output
  )

  seq_df_out(x, ...)
}
