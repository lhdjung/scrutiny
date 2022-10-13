

#' Sequence generation at decimal level
#'
#' @description Functions that provide a smooth interface to generating
#'   sequences based on the input values' decimal depth. Each function creates a
#'   sequence with a step size of one unit on the level of the input values'
#'   ultimate decimal digit (e.g., `2.45`, `2.46`, `2.47`, ...):

#'   - `seq_endpoint()` creates a sequence from one input value to another. For
#'   step size, it goes by the value with more decimal places.
#'   - `seq_distance()` only takes the starting point and, instead of the
#'   endpoint, the desired output length. For step size, it goes by the starting
#'   point by default.
#'
#' `seq_endpoint_df()` and `seq_distance_df()` are variants that create a data
#' frame. Further columns can be added as in `tibble::tibble()`. Regular
#' arguments are the same as in the respective non-`df` function, but with a dot
#' before each.

#' @details If either `from` or `to` ends on zero, be sure to enter that value
#'   as a string! This is crucial because trailing zeros get dropped from
#'   numeric values. A handy way to format numeric values or number-strings
#'   correctly is `restore_zeros()`. The output of the present functions is like
#'   that by default (of `string_output`).
#'
#'   In `seq_endpoint()` and `seq_endpoint_df()`, the step size is determined by
#'   `from` and `to`, whichever has more decimal places. In `seq_distance()` and
#'   `seq_distance_df()`, it's determined by the decimal places of `from`.
#'
#'   These functions are scrutiny's take on `base::seq()`, and themselves
#'   wrappers around it.
#'
#' @param from,.from Numeric (or string coercible to numeric). Starting point of
#'   the sequence.
#' @param to,.to Numeric (or string coercible to numeric). Endpoint of the
#'   sequence. Only in `seq_endpoint()` and `seq_endpoint_df()`.
#' @param by,.by Numeric. Only in `seq_distance()` and `seq_distance_df()`. Step
#'   size of the sequence. If not set, inferred automatically. Default is
#'   `NULL`.
#' @param ... Further columns, added as in `tibble::tibble()`. Only in
#'   `seq_endpoint_df()` and `seq_distance_df()`.
#' @param length_out,.length_out Integer. Length of the output vector (i.e., the
#'   number of its values). Default is `10`. Only in `seq_distance()` and
#'   `seq_distance_df()`.
#' @param dir,.dir Integer. If set to `-1`, the sequence goes backward. Default
#'   is `1`. Only in `seq_distance()` and `seq_distance_df()`.
#' @param offset_from,.offset_from Integer. If set to a non-zero number, the
#'   starting point will be offset by that many units on the level of the last
#'   decimal digit. Default is `0`.
#' @param offset_to,.offset_to Integer. If set to a non-zero number, the
#'   endpoint will be offset by that many units on the level of the last decimal
#'   digit. Default is `0`. Only in `seq_endpoint()` and `seq_endpoint_df()`.
#' @param string_output,.string_output Boolean or string. If `TRUE` (the
#'   default), the output is a string vector. Decimal places are then padded
#'   with zeros to match `from`'s (or `to`'s) number of decimal places. `"auto"`
#'   works like `TRUE` if and only if `from` (`.from`) is a string.
#'
#' @include decimal-places.R restore-zeros.R
#'
#' @return String by default of `string_output`, numeric otherwise.
#'
#' @seealso `seq_disperse()` for sequences centered around the input.
#'
#' @export
#'
#' @examples
#' # Sequence between two points:
#' seq_endpoint(from = 4.7, to = 5)
#'
#' # Sequence of some length; default is 10:
#' seq_distance(from = 0.93)
#' seq_distance(from = 0.93, length_out = 5)
#'
#' # Both of these functions can offset the
#' # starting point...
#' seq_endpoint(from = 14.2, to = 15, offset_from = 4)
#' seq_distance(from = 14.2, offset_from = 4)
#'
#' # ...but only `seq_endpoint()` can offset the
#' # endpoint, because of its `to` argument:
#' seq_endpoint(from = 9.5, to = 10, offset_to = 2)
#'
#' # In return, `seq_distance()` can reverse its direction:
#' seq_distance(from = 20.03, dir = -1)
#'
#' # Both functions have a `_df` variant that returns
#' # a data frame. Arguments are the same but with a
#' # dot, and further columns can be added as in
#' # `tibble::tibble()`:
#' seq_endpoint_df(.from = 4.7, .to = 5, n = 20)
#' seq_distance_df(.from = 0.43, .length_out = 5, sd = 0.08)




seq_endpoint <- function(from, to, offset_from = 0, offset_to = 0,
                         string_output = TRUE) {

  # A number's neighborhood depends on how many decimal places the number has.
  # Therefore, in this function, the step size (`by`) can't be manually chosen
  # as in `seq()`. Instead, it's determined by whichever extreme (starting point
  # or endpoint) has more decimal places:
  digits <- max(decimal_places_scalar(from), decimal_places_scalar(to))
  by <- 1 / (10 ^ digits)

  from_orig <- from
  to_orig   <- to

  type_orig_from <- typeof(from)
  type_orig_to   <- typeof(to)

  # After that, trailing zeros can safely be dropped because `from` and `to` are
  # only relevant in terms of their numeric values:
  from <- as.numeric(from)
  to   <- as.numeric(to)

  # The starting point and/or the endpoint might be offset by some non-zero
  # number of incremental steps. First, the starting point...
  if (offset_from != 0) {
    from <- from + (by * offset_from)
  }

  # ...and then, the endpoint:
  if (offset_to != 0) {
    to <- to + (by * offset_to)
  }

  # If the endpoint is not greater than the starting point, the sequence will go
  # backward. This requires the step size to be negative:
  if (!to > from) {
    by <- -by
  }

  # Generate the sequence:
  out <- suppressWarnings(seq(from = from, to = to, by = by))

  # Hackish way of conveying to `manage_string_output_seq()` whether or not
  # either of `from` and `to` was specified as a string, or else as a double:
  if (is.character(from_orig) | is.character(to_orig)) {
    from <- as.character(from)
  } else if (is.double(from_orig) | is.double(to_orig)) {
    from <- as.double(from)
  } else {
    from <- as.integer(from)
  }

  # Following user preferences, do or don't convert the output to string,
  # restoring trailing zeros to the same number of decimal places that also
  # determined the unit of increments at the start of the function:
  out <- manage_string_output_seq(
    out = out, from = from, string_output = string_output, digits = digits
  )

  return(out)
}




#' @rdname seq_endpoint
#' @export

seq_distance <- function(from, by = NULL, length_out = 10, dir = 1,
                         offset_from = 0, string_output = TRUE) {

  # If the step size by which the sequence progresses (`by`) was not manually
  # chosen as in `seq()`, it is determined by the number of decimal places in
  # `from`:
  if (is.null(by)) {
    digits <- decimal_places_scalar(from)
    by <- 1 / (10 ^ digits)
  } else {
    check_length(by, 1)
    check_type(by, c("integer", "double"))
    digits <- decimal_places_scalar(by)
  }

  # Record if `from` was specified as string; relevant for `string_output`:
  from_orig <- from
  type_orig_from <- typeof(from)

  # After that, trailing zeros can safely be dropped because `from` is only
  # relevant in terms of its numeric value:
  from <- as.numeric(from)

  # The starting point might be offset by some non-zero number of incremental
  # steps (the default is 0, in which case this does nothing):
  if (offset_from != 0) {
    from <- from + (by * offset_from)
  }

  # The distance between the starting point and the end point follows from the
  # step size in conjunction with the desired output length (`length_out`).
  # However, the starting point is also part of the sequence, so it needs to be
  # compensated by subtracting 1 from the desired length:
  distance <- by * (length_out - 1)

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
      "`dir` given as `{dir}`",
      "x" = "`dir` needs to be either `1` or `-1`."
    ))
  }

  # Generate the sequence:
  out <- suppressWarnings(seq(from = from, to = to, by = by))

  # Hackish way of conveying to `manage_string_output_seq()` whether or not
  # `from` was specified as a string:
  from <- methods::as(from, typeof(from_orig))

  # Following user preferences, do or don't convert the output to string,
  # restoring trailing zeros to the same number of decimal places that also
  # determined the unit of increments at the start of the function:
  out <- manage_string_output_seq(
    out = out, from = from, string_output = string_output, digits = digits
  )

  return(out)
}



#' @rdname seq_endpoint
#' @export

seq_endpoint_df <- function(.from, .to, ..., .offset_from = 0, .offset_to = 0,
                            .string_output = TRUE) {

  # Call the basic function to generate the sequence:
  x <- seq_endpoint(
    from = .from, to = .to,
    offset_from = .offset_from, offset_to = .offset_to,
    string_output = .string_output
  )

  # Capture additional columns via tidy evaluation:
  further_cols <- rlang::enexprs(...)

  # Create the resulting tibble (data frame), unquoting and splicing the
  # additional columns into it. Then, add a special class to the tibble, but
  # only to pass messages between (1) here, (2) the testing function, and (3)
  # `seq_test_ranking()`. Finally, return the resulting tibble:
  if (length(further_cols) > 0) {
    out <- tibble::tibble(x, !!!further_cols)
  } else {
    out <- tibble::tibble(x)
  }

  out <- add_class(out, "scr_seq_df")

  return(out)
}



#' @rdname seq_endpoint
#' @export

seq_distance_df <- function(.from, .by = NULL, ..., .length_out = 10, .dir = 1,
                            .offset_from = 0, .string_output = TRUE) {

  # Call the basic function to generate the sequence:
  x <- seq_distance(
    from = .from, by = .by, length_out = .length_out,
    dir = .dir, offset_from = .offset_from,
    string_output = .string_output
  )

  # Capture additional arguments via tidy evaluation:
  further_cols <- rlang::enexprs(...)

  # Create the resulting tibble (data frame), unquoting and splicing the
  # additional columns into it. Then, add a special class to the tibble, but
  # only to pass messages between (1) here, (2) the testing function, and (3)
  # `seq_test_ranking()`. Finally, return the resulting tibble:
  if (length(further_cols) > 0) {
    out <- tibble::tibble(x, !!!further_cols)
  } else {
    out <- tibble::tibble(x)
  }

  out <- add_class(out, "scr_seq_df")

  return(out)
}



