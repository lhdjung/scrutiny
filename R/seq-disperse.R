
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
#' @param string_output,.string_output Boolean or string. If `TRUE`, the output
#'   is a string vector. Decimal places are then padded with zeros to match
#'   `from`'s number of decimal places. Default is `"auto"`, which works like
#'   `TRUE` if and only if `from` (`.from`) is a string.
#' @param include_reported,.include_reported Boolean. Should `from` (`.from`)
#'   itself be part of the sequence built around it? Default is `TRUE` for the
#'   sake of continuity, but this can be misleading if the focus is on the
#'   dispersed values, as opposed to the input.
#'
#' @return
#' @export
#'
#' @seealso Conceptually, `seq_disperse()` is a blend of two function families:
#'   `seq_endpoint()` and `disperse()`.
#'
#' @examples
#' # Basic usage:
#' seq_disperse(from = 4.02)
#'
#' # If trailing zeros matter,
#' # set `string_output` to `TRUE`:
#' seq_disperse(from = 4.02, string_output = TRUE)
#'
#' # Data fame variant:
#' seq_disperse_df(.from = 4.02)
#'
#' # Control steps up and down with
#' # `dispersion` (default is `1:5`):
#' seq_disperse(from = 4.02, dispersion = 1:10)
#' seq_disperse(from = 4.02, dispersion = seq(2, 10, 2))


seq_disperse <- function(from, dispersion = 1:5, string_output = "auto",
                         include_reported = TRUE) {

  check_length(from, 1)

  digits <- decimal_places_scalar(from)
  p10 <- 1 / (10 ^ digits)
  dispersion <- dispersion * p10

  from_num <- as.numeric(from)

  from_minus <- from_num - dispersion
  from_plus  <- from_num + dispersion

  if (include_reported) {
    out <- append(rev(from_minus), c(from_num, from_plus))
  } else {
    out <- append(rev(from_minus), from_plus)
  }

  # Follow user preferences for string output, the latter being necessary to
  # preserve trailing zeros:
  if (string_output == "auto") {
    if (is.character(from)) {
      out <- restore_zeros(out, width = digits)
    }
  } else if (!is.logical(string_output)) {
    if (is.character(string_output)) {
      string_output <- paste0("\"", string_output, "\"")
    }
    cli::cli_abort(c(
      "`string_output` given as `{string_output}`.",
      "x" = "It must be logical or `\"auto\"`."
    ))
  } else if (string_output) {
    out <- restore_zeros(out, width = digits)
  }

  return(out)
}



#' @rdname seq_disperse
#' @export

seq_disperse_df <- function(.from, ..., .dispersion = 1:5,
                            .string_output = "auto",
                            .include_reported = TRUE) {

  x <- seq_disperse(
    from = .from, dispersion = .dispersion,
    string_output = .string_output, include_reported = .include_reported
  )

  further_cols <- rlang::enexprs(...)

  if (length(further_cols) > 0) {
    out <- tibble::tibble(x, !!!further_cols)
  } else {
    out <- tibble::tibble(x)
  }

  return(out)
}


