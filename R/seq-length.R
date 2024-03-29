
#' Set sequence length
#'
#' @description `seq_length()` seamlessly extends or shortens a linear
#'   sequence using the sequence's own step size.
#'
#'   Alternatively, you can directly set the length of a linear sequence in this
#'   way: `seq_length(x) <- value`.
#'
#' @param x Numeric or coercible to numeric. `x` must be linear, i.e., each
#'   of its elements must differ from the next by the same amount.
#' @param value Numeric (whole number, length 1). The new length for `x`.
#'
#' @return A vector of the same type as `x`, with length `value`.
#' - If `value > length(x)`, all original element of `x` are preserved. A number
#' of new elements equal to the difference is appended at the end.
#' - If `value == length(x)`, nothing changes.
#' - If `value < length(x)`, a number of elements of `x` equal to the difference
#' is removed from the end.
#'
#' @export
#'
#' @examples
#' x <- 3:7
#'
#' # Increase the length of `x` from 5 to 10:
#' seq_length(x, 10)
#'
#' # Modify `x` directly (but get
#' # the same results otherwise):
#' seq_length(x) <- 10
#' x
#'
#' # Likewise, decrease the length:
#' x <- 3:7
#' seq_length(x, 2)
#'
#' seq_length(x) <- 2
#' x
#'
#' # The functions are sensitive to decimal levels.
#' # They also return a string vector if (and only if)
#' # `x` is a string vector:
#' x <- seq_endpoint(from = 0, to = 0.5)
#' x
#'
#' seq_length(x, 10)
#'
#' seq_length(x) <- 10
#' x
#'
#' # Same with decreasing the length:
#' seq_length(x, 2)
#'
#' seq_length(x) <- 2
#' x


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

  # Descending sequences have a reverse step size and direction:
  if (length(x) > 1 && x[1L] > x[2L]) {
    by <- -step_size(x)
    dir <- -1
  } else {
    by <- step_size(x)
    dir <- 1
  }

  if (diff < 0L) {
    return(x[1L:(length(x) - abs(diff))])
  }

  extension <- seq_distance(
    from = as.numeric(x[length(x)]) + by,
    by = by, length_out = diff, dir = dir,
    string_output = is.character(x)
  )

  out <- c(x, extension)

  if (is.character(out)) {
    out
  } else {
    methods::as(out, typeof(x))
  }

}



#' @rdname seq_length
#' @export

`seq_length<-` <- function(x, value) {
  seq_length(x, value)
}

