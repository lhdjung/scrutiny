#' Set sequence length
#'
#' @description `seq_length()` extends or shortens a linear sequence to a given
#'   length, using the sequence's own step size. It can also be used as a
#'   replacement function: `seq_length(x) <- value`.
#'
#' @param x Numeric or coercible to numeric. Must be linear.
#' @param value Integer (length 1). The new length.
#'
#' @return A vector of the same type as `x` with length `value`. If `value` is
#'   greater than `length(x)`, new elements are appended; if smaller, elements
#'   are removed from the end.
#'
#' @export
#'
#' @examples
#' x <- 3:7
#' seq_length(x, 10)
#' seq_length(x, 2)
#'
#' # As a replacement function:
#' seq_length(x) <- 10
#' x
#'
#' # The step size and the type follow `x`:
#' x <- seq_endpoint(from = 0, to = 0.5)
#' seq_length(x, 10)

seq_length <- function(x, value) {
  check_type_numeric_like(x)
  x_is_linear <- is_seq_linear(x)

  if (!isTRUE(x_is_linear)) {
    if (is.na(x_is_linear)) {
      cli::cli_abort(c(
        "!" = "`x` in `seq_length(x) <- value` must be a linear sequence.",
        "x" = "Linearity of `x` can't be determined."
      ))
    }
    cli::cli_abort(c(
      "`x` in `seq_length(x) <- value` must be a linear sequence.",
      "x" = "It isn't.",
      "i" = "The length of `x` can only be set if each one \\
      of its elements differs from the next by the same amount."
    ))
  }

  check_length(value, 1L)

  if (!is_whole_number(value)) {
    cli::cli_abort(c(
      "!" = "`value` in `length(x) <- value` must be a whole number.",
      "x" = "It is `{value}`."
    ))
  }

  if (value == 0L) {
    return(vector(mode = typeof(x), length = 0L))
  }

  diff <- value - length(x)

  if (diff == 0L) {
    return(x)
  }

  if (diff < 0L) {
    return(x[1L:(length(x) - abs(diff))])
  }

  # The sequence's own step, as documented -- which is the difference between
  # two of its elements, not the unit of their last decimal place. It used to be
  # the latter, so `seq_length(c(2, 4, 6), 5)` was `2 4 6 7 8`, and a descending
  # sequence was extended *upward*: the sign of the step was applied once here
  # and once more in `seq_distance()`, canceling out. A single element has no
  # step to speak of, and keeps the old rule:
  x_num <- as.numeric(x)
  by <- if (length(x) > 1L) x_num[2L] - x_num[1L] else step_size(x)

  # Rounding back to the decimal level of `x` undoes floating-point error in the
  # multiplication, as `seq_disperse()` does:
  digits <- max(decimal_places(x))
  extension <- round(x_num[length(x)] + by * seq_len(diff), digits)

  if (is.character(x)) {
    return(c(x, restore_zeros(extension, width = digits)))
  }

  methods::as(c(x, extension), typeof(x))
}


#' @rdname seq_length
#' @export

`seq_length<-` <- function(x, value) {
  seq_length(x, value)
}
