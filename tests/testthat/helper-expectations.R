# Test that `object` is what `grim_values()` and `grimmer_values()` return for
# a single undecidable value set: a tibble with one row of `NA` after `case`.
expect_undecidable <- function(object) {
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  if (!tibble::is_tibble(act$val) || nrow(act$val) != 1L) {
    return(testthat::fail(paste(act$lab, "must be a tibble with one row.")))
  }
  if (!identical(act$val$case, 1L) || !all(is.na(unlist(act$val[-1L])))) {
    return(testthat::fail(paste(act$lab, "must be `NA` after `case = 1`.")))
  }

  testthat::pass()
  invisible(act$val)
}


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
