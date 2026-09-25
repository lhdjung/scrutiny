# Test that `object` is a single `NA` (of any type).
expect_na <- function(object) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  if (length(act$val) != 1) {
    return(testthat::fail(paste(act$lab, "must be length 1 (and `NA`).")))
  }
  if (!is.na(act$val)) {
    return(testthat::fail(paste(act$lab, "must be `NA`.")))
  }

  testthat::pass()
  invisible(act$val)
}
