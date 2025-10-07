
# Test that `object` is a single `NA` (of any type).
expect_na <- function(object) {

  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")
  is_na <- length(object) == 1 && is.na(object)

  msg_error <- if (!is_na && length(object) != 1) {
    paste(act$lab, "must be length 1 (and `NA`).")
  } else if (!is_na) {
    paste(act$lab, "must be `NA`.")
  } else {
    ""
  }

  act$is_na <- is_na

  testthat::expect(
    act$is_na,
    msg_error
  )

  invisible(act$val)
}
