#' Remove scrutiny classes
#'
#' Strip any and all scrutiny classes from `x`: those classes that start on
#' `"scr_"`. The function's name follows `base::unclass()`.
#'
#' @param x Any object, but typically a tibble.
#'
#' @return `x`, but without `"scr_"` classes.
#'
#' @noRd
unclass_scr <- function(x) {
  class(x) <- class(x)[!stringr::str_detect(class(x), "^scr_")]
  x
}


#' Censor left and right
#'
#' `censor()` is used in some of scrutiny's unit tests. The `left` and `right`
#' arguments should only be length 1, although this is not checked.
#'
#' @param x Numeric.
#' @param left Numeric. Lower bound. Any elements of `x` that are less than
#'   `left` will be replaced by `left`.
#' @param right Numeric. Upper bound. Any elements of `x` that are greater than
#'   `right` will be replaced by `right`.
#'
#' @return Numeric vector of length `length(x)`.
#'
#' @noRd
censor <- function(x, left, right) {
  x[x < left] <- left
  x[x > right] <- right
  x
}


#' Count integer places
#'
#' Used in unit testing. Analogous to `decimal_places()`.
#'
#' @param x Numeric (or string that can be coerced to numeric). Object with
#'   integer places to count.
#'
#' @return Integer.
#'
#' @noRd
integer_places <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("\\.", n = 2L) %>%
    # TODO: Refactor before switching to base pipe!
    .[, 1L] %>%
    stringr::str_length()
}


#' Remove the integer part, keeping the decimal part
#'
#' `trunc_reverse()` reduces a number to its decimal portion. It is the opposite
#' of `trunc()`: Whereas `trunc(3.45)` returns `3,` `trunc_reverse(3.45)`
#' returns `0.45`.
#'
#' This is used in some unit tests.
#'
#' @param x Decimal number.
#'
#' @return Decimal part of `x`.
#'
#' @noRd
trunc_reverse <- function(x) {
  x - trunc(x)
}

